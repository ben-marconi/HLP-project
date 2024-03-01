module SheetBeautifyHelpers
open BlockHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Optics
open Symbol

// Helper
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let getVisibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and odd integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

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
                (segVecs,[1..segVecs.Length-2])
                ||> List.fold tryCoalesceAboutIndex)



// B1
/// <summary> Read/write the dimensions of a custom component symbol. </summary>
/// <param name="sym">The symbol to access or modify.</param>
/// <returns> A lens for manipulating the symbol's component dimensions. </returns>
let customComponentDimensions_  =
    Lens.create
        (fun (sym : Symbol) ->
        (Option.defaultValue 1.0 sym.HScale) * sym.Component.W, (Option.defaultValue 1.0 sym.VScale) * sym.Component.H)
        (fun (newH, newW) sym ->
        let updatedComponent = { sym.Component with H = newH; W = newW }
        { sym with Component = updatedComponent })


// B2
/// <summary> Updates the position of a symbol on the sheet. </summary>
/// <param name="newPos">The new position for the symbol.</param>
/// <param name="symbol">The symbol to update.</param>
/// <returns>The symbol with its position updated.</returns>
let writeSymbolPos (newPos: XYPos) (symbol: Symbol) : Symbol =
    { symbol with
        Pos = newPos;
        Component = { symbol.Component with
                        X = newPos.X
                        Y = newPos.Y }
        LabelBoundingBox = { symbol.LabelBoundingBox with
                                TopLeft = newPos }
    }



// B3
/// <summary> Provides read/write access to the order of ports on a specified side of a symbol. </summary>
/// <param name="side">The side of the symbol to access the port order for.</param>
/// <returns>A lens for manipulating the port order on the specified side of a symbol.</returns>
let symbolPortOrder_ (side: Edge) : Lens<Symbol, string list> = 
    Lens.create
        (fun (symbol: Symbol) ->
            match Map.tryFind side symbol.PortMaps.Order with
            | Some ports -> ports
            | None -> [] 
        )
        (fun (newOrder: string list) (symbol: Symbol) ->
            let updatedOrder = Map.add side newOrder symbol.PortMaps.Order
            let updatedPortMaps = { symbol.PortMaps with Order = updatedOrder }
            { symbol with PortMaps = updatedPortMaps }
        )

// B4
/// <summary> Read/write the reversed state of the input ports of a MUX2 symbol. </summary>
/// <remarks> Throws an exception if the symbol does not have reversed input ports defined. </remarks>
let reverseInputPorts_ = 
    Lens.create
        (fun (symbol: Symbol) ->
        match symbol.ReversedInputPorts with 
        | None -> failwithf "Reversed Input Ports not found"
        | Some value -> value
        )
        (fun (newReversedInputState: bool) (symbol: Symbol) ->
            { symbol with ReversedInputPorts = Some newReversedInputState }
        )


// B5
/// <summary> Gets the position of a specified port on a symbol within the sheet.</summary>
/// <param name="symbol">The symbol containing the port.</param>
/// <param name="portId">The id of the port whose position is being queried.</param>
/// <returns>The position of the port if found, otherwise None.</returns>
let getPortPosition (model: SymbolT.Model) (portId: string) : XYPos =
    getPortLocation None model portId
    
// B6
/// <summary>Returns the bounding box of a symbol's outline</summary>
/// <param name="symbol">The symbol to calculate the bounding box for.</param>
/// <returns>The bounding box of the symbol. </returns>
let getBoundingBox (symbol: Symbol) = 
    let h,w = getRotatedHAndW symbol
    if symbol.Annotation = Some ScaleButton then 
        {TopLeft = symbol.Pos - {X = 9.; Y = 9.}; H = 17. ; W = 17.}
    else 
        {TopLeft = symbol.Pos; H = float(h) ; W = float(w)}

// B7
/// <summary>Reads or writes the rotation state of a symbol.</summary>
/// <param name="symbol">The symbol to modify or read the rotation from.</param>
/// <param name="rotation">The new rotation state to apply to the symbol.</param>
/// <returns>The updated symbol with the new rotation state.</returns>
let symbolRotation_ = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (rotation: Rotation) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Rotation = rotation}
        { symbol with STransform = newSTransform })


// B8
/// <summary>Reads or writes the flip state of a symbol.</summary>
/// <param name="symbol">The symbol to modify or read the flip state from.</param>
/// <param name="newFlip">The new flip state to apply to the symbol.</param>
/// <returns>The updated symbol with the new flip state.</returns>
let symbolFlip_ = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Flipped)
        (fun (newFlip: bool) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Flipped = newFlip}
        { symbol with STransform = newSTransform })


// T1
/// <summary>Counts the number of pairs of symbols that intersect each other on a sheet.</summary>
/// <param name="sheet">The sheet model containing the symbols.</param>
/// <returns>The total number of intersecting symbol pairs.</returns>
let countIntersectingSymbolPairs (sheet: SheetT.Model) : int =
    let boxes =
        Map.values sheet.BoundingBoxes
        |> Seq.toList
        |> List.mapi (fun n box -> n, box)

    let intersectingPairs =
        List.allPairs boxes boxes
        |> List.filter (fun ((n1, box1), (n2, box2)) -> n1 <> n2 && BlockHelpers.overlap2DBox box1 box2)

    intersectingPairs.Length


// T2
/// <summary>Counts the number of distinct wire visible segments that intersect with one or more symbols on a sheet.</summary>
/// <param name="model">The sheet model containing the wires and symbols.</param>
/// <returns>The total number of visible wire segment intersections with symbols.</returns>
let countSegmentSymbolIntersections (model: SheetT.Model): int =
    let convertVisibleSegmentsToSegments (wire : Wire) (posList : XYPos list) : Segment list = 
        let getLength (pos : XYPos) : float =
            match pos.X, pos.Y with 
            | 0., y -> y
            | x, 0. -> x
            | _ -> failwithf "visibleSegment has wrong form"

        posList
        |> List.mapi (fun i pos -> 
                        {Index =  i;
                         Length = getLength pos;
                         WireId = wire.WId;
                        // Other attributes don't matter for this function so set to random values
                         IntersectOrJumpList = [0.];
                         Draggable = true;
                         Mode = Auto; })

    let getInitialOrientation (posList : XYPos list) : Orientation = 
        let firstPos = List.head posList
        match firstPos.X, firstPos.Y with
        | 0., _ -> Vertical
        | _  -> Horizontal

    // Iterate through all wires, updating each with its visible segments
    let updatedModelWithNewWires  =
        model.Wire.Wires
        |> Map.toList
        |> List.map (fun (wireId, wire) ->
            let visibleSegmentsXY = getVisibleSegments wireId model
            let visibleSegments = convertVisibleSegmentsToSegments wire visibleSegmentsXY
            let newInitialOrientation = getInitialOrientation visibleSegmentsXY
            // Construct a new wire record with updated segments
            let updatedWire = { wire with Segments = visibleSegments; InitialOrientation = newInitialOrientation }
            (wireId, updatedWire))
        |> Map.ofList
        // Construct a new model with the updated wires
        |> fun updatedWires -> { model with Wire = { model.Wire with Wires = updatedWires } }
    
    model.Wire.Wires
    |> Map.toList
    |> List.map (fun (wireId, wire) -> 
                    BusWireRoute.findWireSymbolIntersections updatedModelWithNewWires.Wire wire )
    |> List.map List.length
    |> List.fold (+) 0

// T3
/// <summary>Counts the number of distinct pairs of segments that cross each other at right angles on a sheet.</summary>
/// <param name="model">The sheet model for which to count the right angle segment intersections.</param>
/// <returns>The total number of right angle intersections between segments.</returns>
let countRightAngleSegmentIntersections (model: SheetT.Model) : int =
    // Extract wires from model
    let wires = model.Wire.Wires

    // Function to check whether two segments intersect
    let segmentsIntersect (segment1: ASegment) (segment2: ASegment) : bool =
        let xIntersects = overlap1D (segment1.Start.X, segment1.End.X) (segment2.Start.X, segment2.End.X)
        let yIntersects = overlap1D (segment1.Start.Y, segment1.End.Y) (segment2.Start.Y, segment2.End.Y)
        xIntersects && yIntersects

    // Function to check whether two segments are perpendicular
    let areSegmentsPerpendicular (segment1: ASegment) (segment2: ASegment) : bool =
        match segment1.Orientation, segment2.Orientation with
        | Horizontal, Vertical | Vertical, Horizontal -> true
        | _ -> false

    // Function to check if segments are on the same net
    let areSegmentsOnDifferentNet (segment1: ASegment) (segment2: ASegment) : bool =
        let _, wireId1 = segment1.GetId
        let _, wireId2 = segment2.GetId
        let wire1 = Map.find wireId1 wires
        let wire2 = Map.find wireId2 wires
        wire1.OutputPort <> wire2.OutputPort
    
    // List of segments
    let wireSegments = 
        wires 
        |> Map.values
        |> Seq.collect (fun wire -> Seq.toList (getNonZeroAbsSegments wire))
        |> List.ofSeq
    
    // Check the conditions on all pairs of segments
    List.allPairs wireSegments wireSegments
    |> List.filter (fun (seg1, seg2) -> 
                        // Check intersection, right angle and that the segments are on different nets
                        segmentsIntersect seg1 seg2 && 
                        areSegmentsPerpendicular seg1 seg2 && 
                        areSegmentsOnDifferentNet seg1 seg2
                        )
    |> List.distinct
    |> List.length
    |> (fun x -> x / 2) // Divide by two because each intersection was counted twice



// T4
/// <summary>Calculates the total visible length of wiring segments on the sheet, counting overlaps on the same net as one.</summary>
/// <param name="model">The sheet model containing wiring segments.</param>
/// <returns>The total length of visible wiring segments.</returns>
let totalSegmentLength (model: SheetT.Model) : int =
    let wires = model.Wire.Wires

    let getSegmentLength (segment: ASegment) : int =
        int segment.Segment.Length

    let areSegmentsOverlapping (segment1: ASegment) (segment2: ASegment) : bool =
        match segment1.Orientation, segment2.Orientation with
        | Horizontal, Horizontal -> overlap1D (segment1.Start.X, segment1.End.X) (segment2.Start.X, segment2.End.X)
        | Vertical, Vertical -> overlap1D (segment1.Start.Y, segment1.End.Y) (segment2.Start.Y, segment2.End.Y)
        | _ -> false // No overlap if orientations are different

    let areSegmentsOnSameNet (segment1: ASegment) (segment2: ASegment) : bool =
        let _, wireId1 = segment1.GetId
        let _, wireId2 = segment2.GetId
        let wire1 = Map.find wireId1 wires
        let wire2 = Map.find wireId2 wires
        wire1.OutputPort = wire2.OutputPort

    let removeShortestOverlappingSegment segments =
        segments
        |> List.fold (fun acc segment ->
            let overlappingSegment = acc |> List.tryFind (fun otherSegment ->
                areSegmentsOverlapping segment otherSegment && areSegmentsOnSameNet segment otherSegment)
            
            match overlappingSegment with
            | Some(other) when getSegmentLength segment > getSegmentLength other ->
                // If the current segment is longer, replace the overlapping one with the current segment
                List.filter (fun s -> s <> other) acc @ [segment]
            | None ->
                // If there is no overlapping segment, or the overlapping segment is longer, add the current segment if it's not already included
                if not (List.contains segment acc) then acc @ [segment] else acc
            | _ -> acc // If the overlapping segment is longer, do nothing
            ) []

    // Flatten all segments into a list
    let allSegments = 
        wires 
        |> Map.values
        |> Seq.collect (fun wire -> getNonZeroAbsSegments wire)
        |> Seq.toList

    // Group segments by net and apply deduplication
    let uniqueSegments = 
        allSegments
        |> List.groupBy (fun segment ->
            let _, wireId = segment.GetId
            let wire = Map.find wireId wires
            wire.OutputPort)
        |> List.collect (fun (_, segmentsOnSameNet) -> removeShortestOverlappingSegment segmentsOnSameNet)

    // Calculate the total length
    uniqueSegments
    |> List.sumBy getSegmentLength

    


// T5
/// <summary>Counts the number of visible wire right-angles over the whole sheet.</summary>
/// <param name="model">The sheet model to count for visible right-angles in wires.</param>
/// <returns>The total number of visible wire right-angle.</returns>
let countWireRightAngles (model: SheetT.Model) : int = 
    let getNumRightAngles (posList : XYPos list) : int = 
        List.length posList - 1

    model.Wire.Wires
    |> Map.toList
    |> List.map (fun (wireId, wire) ->
        let visibleSegmentsXY = getVisibleSegments wireId model
        getNumRightAngles visibleSegmentsXY)
    |> List.fold (+) 0


