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

//Helper function from Tick 3
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
        if segVecs[index] =~ XYPos.zero then
            segVecs[0 .. index - 2]
            @ [ segVecs[index - 1] + segVecs[index + 1] ]
            @ segVecs[index + 2 .. segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
        (segVecs, [ 1 .. segVecs.Length - 2 ])
        ||> List.fold tryCoalesceAboutIndex)

//B1R, B1W
let customComponentSymbolDimensions_ =
    Lens.create
        (fun (sym: Symbol) ->
            (Option.defaultValue 1.0 sym.HScale)
            * sym.Component.W,
            (Option.defaultValue 1.0 sym.VScale)
            * sym.Component.H)
        (fun (updatedH: float, updatedW: float) (sym: Symbol) -> setCustomCompHW updatedH updatedW sym)

//B2W
let updateSymbolPos (newPos: XYPos) (symbol: Symbol) : Symbol =
    { symbol with
        Pos = newPos
        Component = { symbol.Component with X = newPos.X; Y = newPos.Y }
        LabelBoundingBox = { symbol.LabelBoundingBox with TopLeft = newPos } }

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

// B4R, B4W
let reverseInputPortsMUX2_ =
    Lens.create
        (fun (symbol: Symbol) ->
            match symbol.ReversedInputPorts with
            | Some value -> value
            | None -> failwithf "Reversed Input Ports not found")
        (fun (updatedInputState: bool) (symbol: Symbol) -> { symbol with ReversedInputPorts = Some updatedInputState })

// B5R

let portPos (symbolModel: SymbolT.Model) (portId: string) = getPortLocation None symbolModel portId

//B6

let getSymbolBoundingBox (symbol: Symbol) = getSymbolBoundingBox symbol

// B7R, B7W
let symbolRotationState_ =
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (updatedRotation: Rotation) (symbol: Symbol) ->
            { symbol with
                STransform = { symbol.STransform with Rotation = updatedRotation } })

// B8R, B8W
let symbolFlipState_ =
    Lens.create (fun (symbol: Symbol) -> symbol.STransform.Flipped) (fun (updatedFlip: bool) (symbol: Symbol) ->
        { symbol with STransform = { symbol.STransform with Flipped = updatedFlip } })

let countSymbolIntersectsSymbol (sheet: SheetT.Model) : int =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n, box)
    List.allPairs boxes boxes
    |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length

//////Fix up/////
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

let getSymbolsBoundingBox (model: SheetT.Model) : BoundingBox list =
    model.Wire.Symbol.Symbols
    |> Map.values
    |> Seq.map getSymbolBoundingBox
    |> Seq.toList
let countWireSegmentsIntersectingSymbols (model: SheetT.Model) : int =
    let symbolsBoundingBoxes = getSymbolsBoundingBox model

    let segmentIntersectsAnySymbol (segmentStart: XYPos) (segmentEnd: XYPos) : bool =
        symbolsBoundingBoxes
        |> List.exists (fun bb ->
            match segmentIntersectsBoundingBox bb segmentStart segmentEnd with
            | Some _ -> true
            | None -> false)

    model.Wire.Wires
    |> Map.fold
        (fun acc wId wire ->
            let segmentPositions = calculateAbsolutePositions wire model
            let intersections =
                segmentPositions
                |> List.fold
                    (fun count (startPos, endPos) ->
                        if segmentIntersectsAnySymbol startPos endPos then
                            count + 1
                        else
                            count)
                    0

            acc + intersections)
        0

//ASegment.Orientation
let countRightAngleCrossings (model: Model) : int =
    // Get all segments with their absolute start and end positions
    let allASegments =
        model.Wire.Wires
        |> Map.fold
            (fun acc _ wire ->
                let rec calculatePositions (segs: Segment list) (currentPos: XYPos) (acc: ASegment list) =
                    match segs with
                    | [] -> acc
                    | seg :: tail when not seg.IsZero ->
                        let newPos =
                            match wire.InitialOrientation with
                            | Horizontal when seg.Index % 2 = 0 -> { X = currentPos.X + seg.Length; Y = currentPos.Y }
                            | Vertical when seg.Index % 2 = 1 -> { X = currentPos.X; Y = currentPos.Y + seg.Length }
                            | _ -> currentPos
                        calculatePositions
                            tail
                            newPos
                            ({ Start = currentPos; End = newPos; Segment = seg }
                             :: acc)
                    | _ :: tail -> calculatePositions tail currentPos acc
                calculatePositions wire.Segments wire.StartPos [])
            []

    let doIntersect (aSeg: ASegment) (bSeg: ASegment) : bool =
        let aOrientation = aSeg.Orientation
        let bOrientation = bSeg.Orientation
        match aOrientation, bOrientation with
        | Horizontal, Vertical ->
            aSeg.Start.Y >= (min bSeg.Start.Y bSeg.End.Y)
            && aSeg.Start.Y <= (max bSeg.Start.Y bSeg.End.Y)
            && bSeg.Start.X >= (min aSeg.Start.X aSeg.End.X)
            && bSeg.Start.X <= (max aSeg.Start.X aSeg.End.X)
        | Vertical, Horizontal ->
            aSeg.Start.X >= (min bSeg.Start.X bSeg.End.X)
            && aSeg.Start.X <= (max bSeg.Start.X bSeg.End.X)
            && bSeg.Start.Y >= (min aSeg.Start.Y aSeg.End.Y)
            && bSeg.Start.Y <= (max aSeg.Start.Y aSeg.End.Y)
        | _ -> false

    let segmentPairs =
        allASegments
        |> List.collect (fun aSeg ->
            allASegments
            |> List.choose (fun bSeg ->
                if aSeg.Segment.WireId <> bSeg.Segment.WireId then
                    Some(aSeg, bSeg)
                else
                    None))

    segmentPairs
    |> List.filter (fun (aSeg, bSeg) -> doIntersect aSeg bSeg)
    |> List.length

let sumOfWireSegmentsLength (model: SheetT.Model) : float =
    // Function to calculate the length of a segment from its start and end points
    let segmentLength ((startPos, endPos): XYPos * XYPos) =
        let dx = endPos.X - startPos.X
        let dy = endPos.Y - startPos.Y
        sqrt (dx * dx + dy * dy)

    // Retrieve all segments' absolute positions and calculate their lengths
    let allSegmentsLengths =
        model.Wire.Wires
        |> Map.fold
            (fun acc _ wire ->
                let segmentPositions = calculateAbsolutePositions wire model
                let lengths = segmentPositions |> List.map segmentLength
                acc @ lengths)
            []

    // Sum the lengths, ensuring that identical lengths (from overlapping segments) are counted only once
    allSegmentsLengths
    |> Seq.countBy id // Group by identical lengths
    |> Seq.map snd // Take the counts
    |> Seq.sum // Sum the lengths

let countVisibleWireRightAngles (model: Model) : int =
    let countRightAnglesInWire (wire: Wire) : int =
        let segVecs = visibleSegments wire.WId model
        // Every segment is a right angle so we just count the segments.
        // Subtract 1 because the last segment does not contribute to an additional right angle.
        max (List.length segVecs - 1) 0

    model.Wire.Wires
    |> Map.fold (fun acc _ wire -> acc + countRightAnglesInWire wire) 0
