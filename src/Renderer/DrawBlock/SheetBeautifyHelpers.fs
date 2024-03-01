module SheetBeautifyHelpers

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open BlockHelpers
open Optics
open Helpers
open Symbol

///Helper function from Tick 3
let visibleSegments (wId: ConnectionId) (model: SheetT.Model) : XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) =
        match n % 2 with
        | 0 -> IsEven
        | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index: int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical
        | IsOdd, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
        | IsEven, BusWireT.Horizontal
        | IsOdd, BusWireT.Vertical -> { X = seg.Length; Y = 0. }

    /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// if this is possible, otherwise return segVecs unchanged.
    /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
        if index > 0 && index < segVecs.Length - 1 then
            if segVecs[index] =~ XYPos.zero then
                segVecs[0 .. index - 2]
                @ [ segVecs[index - 1] + segVecs[index + 1] ]
                @ segVecs[index + 2 .. segVecs.Length - 1]
            else
                segVecs
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
        (segVecs, [ 1 .. segVecs.Length - 2 ])
        ||> List.fold tryCoalesceAboutIndex)

/// Lens that reads and writes the dimensions of a custom component symbol.
//B1R, B1W
let customComponentSymbolDimensions_ =
    Lens.create
        (fun (sym: Symbol) ->
            (Option.defaultValue 1.0 sym.HScale)
            * sym.Component.W,
            (Option.defaultValue 1.0 sym.VScale)
            * sym.Component.H)
        (fun (updatedH: float, updatedW: float) (sym: Symbol) -> setCustomCompHW updatedH updatedW sym)

/// Updates symbol position using new XYPos
//B2W
let updateSymbolPos (newPos: XYPos) (symbol: Symbol) : Symbol =
    { symbol with
        Pos = newPos
        Component = { symbol.Component with X = newPos.X; Y = newPos.Y }
        LabelBoundingBox = { symbol.LabelBoundingBox with TopLeft = newPos } }

/// Lens that reads and writes the order of ports for a symbol.
// B3R, B3W
let symbolPortOrder_ (edge: Edge) : Lens<Symbol, string list> =
    Lens.create
        (fun (symbol: Symbol) ->
            match Map.tryFind edge symbol.PortMaps.Order with
            | Some ports -> ports
            | None -> [])
        (fun (newOrder: string list) (symbol: Symbol) ->
            let updatedPortMapsOrder = Map.add edge newOrder symbol.PortMaps.Order
            { symbol with PortMaps = { symbol.PortMaps with Order = updatedPortMapsOrder } })

/// Lense that reads and writes the reverse state of MUX2 inputs
// B4R, B4W
let reverseInputPortsMUX2_ =
    Lens.create
        (fun (symbol: Symbol) ->
            match symbol.ReversedInputPorts with
            | Some value -> value
            | None -> failwithf "Reversed Input Ports not found")
        (fun (updatedInputState: bool) (symbol: Symbol) -> { symbol with ReversedInputPorts = Some updatedInputState })

/// Reads the position of on a sheet
// B5R
let portPos (symbolModel: SymbolT.Model) (portId: string) = getPortLocation None symbolModel portId

/// Reads the BoundingBox of a symbol
//B6
let getSymbolBoundingBox (symbol: Symbol) = getSymbolBoundingBox symbol

/// Lens that reads/writes a symbol's rotation state from its STransform.
// B7R, B7W
let symbolRotationState_ =
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (updatedRotation: Rotation) (symbol: Symbol) ->
            { symbol with
                STransform = { symbol.STransform with Rotation = updatedRotation } })

/// Lens that reads/writes the flip state of a symbol
// B8R, B8W
let symbolFlipState_ =
    Lens.create (fun (symbol: Symbol) -> symbol.STransform.Flipped) (fun (updatedFlip: bool) (symbol: Symbol) ->
        { symbol with STransform = { symbol.STransform with Flipped = updatedFlip } })

/// Number of of pairs of symbols that intersect each other
// T1R
let countSymbolIntersectsSymbol (sheet: SheetT.Model) : int =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n, box)
    List.allPairs boxes boxes
    |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length

/// Converts list of segment directional vectors to vector of segment start and end points on the sheet
/// Used in countWireSegmentsIntersectingSymbols to find coordinates of visible segments
let calculateAbsolutePositions (wire: BusWireT.Wire) (model: SheetT.Model) : list<XYPos * XYPos> =
    let initialPos: XYPos = wire.StartPos
    let visibleSegs: XYPos list = visibleSegments wire.WId model
    visibleSegs
    |> List.fold
        (fun (lastPos: XYPos, accum: list<XYPos * XYPos>) segVec ->
            let newPos: XYPos = { X = lastPos.X + segVec.X; Y = lastPos.Y + segVec.Y }
            (newPos, (lastPos, newPos) :: accum))
        (initialPos, [])
    |> snd
    |> List.rev

/// Obtains list of symbol bounding boxes from a model
let getSymbolsBoundingBox (model: SheetT.Model) : BoundingBox list =
    model.Wire.Symbol.Symbols
    |> Map.values
    |> Seq.map getSymbolBoundingBox
    |> Seq.toList

/// Number of visible segments that intersect with one or more symbols in a sheet
// T2R
let countWireSegmentsIntersectingSymbols (model: SheetT.Model) : int =
    let symbolsBoundingBoxes = getSymbolsBoundingBox model

    let sumIncludingPorts =
        model.Wire.Wires
        |> Map.fold
            (fun acc _ wire ->
                let segmentPositions = calculateAbsolutePositions wire model
                let intersections =
                    segmentPositions
                    |> List.fold
                        (fun count (startPos, endPos) ->
                            let doesIntersect =
                                symbolsBoundingBoxes
                                |> List.exists (fun bb ->
                                    match segmentIntersectsBoundingBox bb startPos endPos with
                                    | Some _ -> true
                                    | None -> false)
                            if doesIntersect then
                                count + 1
                            else
                                count)
                        0

                acc + intersections)
            0
    sumIncludingPorts - ((model.Wire.Wires).Count * 2)

/// Check whether segments are in the same net
let segSameNetCheck seg1 seg2 model =
    let wire1 = model.Wire.Wires |> Map.find seg1.WireId
    let wire2 = model.Wire.Wires |> Map.find seg2.WireId
    wire1.OutputPort = wire2.OutputPort

/// Checks whether semgents overlap
let segOverlapCheck (seg1: ASegment) (seg2: ASegment) =
    overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X)
    && overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)

/// Checks whether segments are right angles. Avoids duplicates by matching false for reverse cases.
let segRightAngleCheck (seg1: ASegment) (seg2: ASegment) =
    match seg1.Orientation, seg2.Orientation with
    | Horizontal, Vertical -> false
    | Vertical, Horizontal -> segOverlapCheck seg1 seg2
    | _ -> false

/// Number of distinct pairs of segments that cross each other at right angles.
/// Not including 0 length segments/segments on the same net intersecting/segments on same net on top of each other
// T3R
let countRightAngleCrossings (model: SheetT.Model) : int =
    let allSegments =
        model.Wire.Wires
        |> Map.values
        |> Seq.collect (fun wire -> getNonZeroAbsSegments wire |> Seq.toList)
        |> List.ofSeq

    let rightAngleIntersections =
        List.allPairs allSegments allSegments
        |> List.filter (fun (seg1, seg2) -> (segRightAngleCheck seg1 seg2))
        |> List.filter (fun (seg1, seg2) -> not (segSameNetCheck seg1.Segment seg2.Segment model))

    rightAngleIntersections.Length

/// Total length of all segments in a model, excluding same-net overlap.
// T4R
let sumOfUniqueWireSegmentsLengths (model: SheetT.Model) : float =
    let totalLengthVisibleSeg (wire: Wire) : float =
        let visibleSegVecs = visibleSegments wire.WId model
        let totalX =
            visibleSegVecs
            |> List.fold (fun acc pos -> acc + pos.X) 0.0
        let totalY =
            visibleSegVecs
            |> List.fold (fun acc pos -> acc + pos.Y) 0.0
        totalX + totalY
    model.Wire.Wires
    |> Map.map (fun _ wire -> totalLengthVisibleSeg wire)
    |> Map.fold (fun acc _ length -> acc + length) 0.0

/// Number of visible wire right-angles for a whole sheet
// T5R
let countVisibleWireRightAngles (model: SheetT.Model) : int =
    let countRightAnglesInWire (wire: Wire) : int =
        let segVecs = visibleSegments wire.WId model
        // Every segment is a right angle so we just count the segments.
        // Subtract 1 because the last segment does not contribute to an additional right angle.
        max (List.length segVecs - 1) 0

    model.Wire.Wires
    |> Map.fold (fun acc _ wire -> acc + countRightAnglesInWire wire) 0
/// Finds all segments that retarce and a list of all the end of wire segments  that retrace so
/// far that the next segment (index = 3 or Segments.Length – 4) - starts inside a symbol.
let findRetracingSegments (model: SheetT.Model) =
    // Checks if the end of a wire retraces into a symbol
    let isPointWithinBoundingBox (point: XYPos) (bbox: BoundingBox) =
        let inXRange =
            point.X >= bbox.TopLeft.X
            && point.X <= bbox.TopLeft.X + bbox.W
        let inYRange =
            point.Y >= bbox.TopLeft.Y
            && point.Y <= bbox.TopLeft.Y
        inXRange && inYRange
    let endRetracesIntoSymbol wire =
        let lastIndex = (List.length wire.Segments) - 1
        let lastSegment = List.item lastIndex wire.Segments
        let nextToLastIndex = lastIndex - 3
        let nextToLastSegment = List.item nextToLastIndex wire.Segments
        let startPos =
            calculateAbsolutePositions wire model
            |> List.head
            |> fst
        let endPos =
            calculateAbsolutePositions wire model
            |> List.last
            |> snd
        let symbolStartPosWithin =
            model.Wire.Symbol.Symbols
            |> Map.exists (fun _ symbol ->
                let bbox = getSymbolBoundingBox symbol
                isPointWithinBoundingBox startPos bbox
                || isPointWithinBoundingBox endPos bbox)
        (symbolStartPosWithin,
         nextToLastSegment.Length < 0.0
         || lastSegment.Length < 0.0)

    // Finds retracing segments
    let retracingSegments, endRetracingSegments =
        model.Wire.Wires
        |> Map.fold
            (fun (retracing, endRetracing) _ wire ->
                let zeroLengthIdxs =
                    wire.Segments
                    |> List.indexed
                    |> List.choose (fun (idx, seg) ->
                        if seg.Length = 0.0 then
                            Some idx
                        else
                            None)
                let retracingSegmentsForWire =
                    zeroLengthIdxs
                    |> List.choose (fun idx ->
                        if idx > 0 && idx < List.length wire.Segments - 1 then
                            let before = List.item (idx - 1) wire.Segments
                            let after = List.item (idx + 1) wire.Segments
                            if before.Length * after.Length < 0.0 then
                                Some idx
                            else
                                None
                        else
                            None)
                let endRetracingCheck = endRetracesIntoSymbol wire
                (retracing @ retracingSegmentsForWire, endRetracing @ [ endRetracingCheck ]))
            ([], [])

    (retracingSegments, endRetracingSegments)
