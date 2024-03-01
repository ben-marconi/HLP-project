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
let countRightAngleIntersectingSegments (model: SheetT.Model) : int =
    let allSegments =
        model.Wire.Wires
        |> Map.fold (fun acc _ wire -> acc @ calculatePositions wire) []

    let segmentsIntersectAtRightAngles (seg1: ASegment) (seg2: ASegment) =
        match seg1.Orientation, seg2.Orientation with
        | Horizontal, Vertical
        | Vertical, Horizontal ->
            let horizontalSegment =
                if seg1.Orientation = Horizontal then
                    seg1
                else
                    seg2
            let verticalSegment =
                if seg1.Orientation = Vertical then
                    seg1
                else
                    seg2
            // Check for overlap in both dimensions
            overlap1D
                (horizontalSegment.Start.Y, horizontalSegment.End.Y)
                (verticalSegment.Start.Y, verticalSegment.End.Y)
            && overlap1D
                (horizontalSegment.Start.X, horizontalSegment.End.X)
                (verticalSegment.Start.X, verticalSegment.End.X)
        | _ -> false

    // Check for intersections between all unique pairs of segments
    allSegments
    |> List.collect (fun seg1 -> allSegments |> List.map (fun seg2 -> seg1, seg2))
    |> List.filter (fun (seg1, seg2) ->
        // Ensure we're not comparing a segment with itself
        seg1.Segment.Index <> seg2.Segment.Index
        || seg1.Segment.WireId <> seg2.Segment.WireId)
    |> List.filter (fun (seg1, seg2) -> segmentsIntersectAtRightAngles seg1 seg2)
    |> List.length

let sumOfUniqueWireSegmentsLengths (model: SheetT.Model) : float =
    let segmentLength (startPos: XYPos, endPos: XYPos) =
        let dx = endPos.X - startPos.X
        let dy = endPos.Y - startPos.Y
        sqrt (dx * dx + dy * dy)

    let segmentIdentifier (startPos: XYPos, endPos: XYPos) =
        if startPos.X = endPos.X then
            sprintf "V%f[%f,%f]" startPos.X (min startPos.Y endPos.Y) (max startPos.Y endPos.Y)
        else
            sprintf "H%f[%f,%f]" startPos.Y (min startPos.X endPos.X) (max startPos.X endPos.X)

    let addUniqueSegment (acc: (string * float) list) (segment: (XYPos * XYPos)) =
        let id = segmentIdentifier segment
        let len = segmentLength segment
        if
            acc
            |> List.exists (fun (existingId, _) -> existingId = id)
        then
            acc
        else
            (id, len) :: acc

    let uniqueSegmentsLengths =
        model.Wire.Wires
        |> Map.fold
            (fun acc _ wire ->
                let segmentPositions = calculateAbsolutePositions wire model
                List.fold addUniqueSegment acc segmentPositions)
            []
    uniqueSegmentsLengths |> List.map snd |> List.sum

let countVisibleWireRightAngles (model: Model) : int =
    let countRightAnglesInWire (wire: Wire) : int =
        let segVecs = visibleSegments wire.WId model
        // Every segment is a right angle so we just count the segments.
        // Subtract 1 because the last segment does not contribute to an additional right angle.
        max (List.length segVecs - 1) 0

    model.Wire.Wires
    |> Map.fold (fun acc _ wire -> acc + countRightAnglesInWire wire) 0
