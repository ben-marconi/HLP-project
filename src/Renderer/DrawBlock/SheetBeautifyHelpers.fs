module SheetBeautifyHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics
open Symbol

// Helper
 /// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

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
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if segVecs[index] =~ XYPos.zero
            then
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
                (segVecs,[1..segVecs.Length-2])
                ||> List.fold tryCoalesceAboutIndex)



// B1
// Read/write the dimensions of a custom comp symbol
let customComponentDimensions_  =
    Lens.create
        (fun (sym : Symbol) ->
        (Option.defaultValue 1.0 sym.HScale) * sym.Component.W, (Option.defaultValue 1.0 sym.VScale) * sym.Component.H)
        (fun (newH, newW) sym ->
        let updatedComponent = { sym.Component with H = newH; W = newW }
        { sym with Component = updatedComponent })

// B2
// Write the position of a symbol on the sheet
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
// Read/write the order of ports on a specified side of a symbol
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
// Read/write the reverses state of the inputs of a MUX2
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
// Read the position of a port on the sheet. It cannot directly be written
let getPortPosition (symbol: Symbol) (portId: string) : Option<XYPos> =
    let comp = symbol.Component
    let maybeEdge = symbol.PortMaps.Orientation |> Map.tryFind portId
    let portsOnEdge = maybeEdge |> Option.bind (fun edge -> symbol.PortMaps.Order |> Map.tryFind edge)
    match maybeEdge, portsOnEdge with
    | Some edge, Some ports ->
        let index = ports |> List.tryFindIndex (fun id -> id = portId)
        match index with
        | None -> None
        | Some index ->
            let gap, basePos = 
                match edge with
                | Top -> comp.W / float (List.length ports + 1), comp.Y
                | Bottom -> comp.W / float (List.length ports + 1), comp.Y + comp.H
                | Left -> comp.H / float (List.length ports + 1), comp.X
                | Right -> comp.H / float (List.length ports + 1), comp.X + comp.W
            let offset = gap * float (index + 1)
            match edge with
            | Top | Bottom -> Some { X = comp.X + offset; Y = basePos }
            | Left | Right -> Some { X = basePos; Y = comp.Y + offset }
    | _ -> None
    
// B6
// The Bounding box of a symbol outline (position is contained in this)
let getBoundingBox (symbol: Symbol) = 
    let h,w = getRotatedHAndW symbol
    if symbol.Annotation = Some ScaleButton then 
        {TopLeft = symbol.Pos - {X = 9.; Y = 9.}; H = 17. ; W = 17.}
    else 
        {TopLeft = symbol.Pos; H = float(h) ; W = float(w)}

// B7
// Read/Write The rotation state of a symbol
let symbolRotation_ = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (rotation: Rotation) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Rotation = rotation}
        { symbol with STransform = newSTransform })


// B8
// Read/Write The flip state of a symbol
let symbolFlip_ = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Flipped)
        (fun (newFlip: bool) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Flipped = newFlip}
        { symbol with STransform = newSTransform })


// T1
// The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols.
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

// Adapted function to find intersections between wire segments and symbols using XYPos list
// Adapted function to find intersections between wire segments and symbols using XYPos list and SymbolT.Model
(*
let findWireSymbolIntersections (model: SymbolT.Model) (segmentPositions: XYPos list) : BoundingBox list =
    let allSymbolsIntersected =
        model.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (s.Component.Type, Symbol.getSymbolBoundingBox s))



// The number of distinct wire visible segments that intersect with one or more symbols.
// Count over all visible wire segments.
let countVisibleSegmentIntersections (model: SheetT.Model): int =
    let convertXYPosListToSegments (visibleSegments: XYPos list) : list<Segment> =
    // Implementation depends on how I map XYPos back to Segments
    []

    // Iterate through all wires, updating each with its visible segments
    let updateWiresWithVisibleSegments (model: SheetT.Model) : SheetT.Model =
        model.Wire.Wires
        |> Map.toList
        |> List.map (fun (wireId, wire) ->
            let visibleSegmentsXY = visibleSegments wireId model
            let visibleSegments = convertVisibleSegmentsToSegments visibleSegmentsXY
            // Construct a new wire record with updated segments
            let updatedWire = { wire with Segments = visibleSegments }
            (wireId, updatedWire))
        |> Map.ofList
        // Construct a new model with the updated wires
        |> fun updatedWires -> { model with Wire = { model.Wire with Wires = updatedWires } }
    
*)

// T3
// The number of distinct pairs of segments that cross each other at right angles. 
// Does not include 0 length segments or segments on same net intersecting at one end, or
// segments on same net on top of each other. Count over whole sheet.
let countRightAngleIntersections (model: SheetT.Model) : int =
    // Extract wires from model
    let wires = model.Wire.Wires |> Map.toSeq |> Seq.map snd
    
    // Map of WireId to non-zero absolute segments
    let wireSegmentsMap = 
        wires 
        |> Seq.map (fun wire -> wire.WId, BlockHelpers.getNonZeroAbsSegments wire)
        |> Map.ofSeq

    let intersects (hSegment: ASegment) (vSegment: ASegment) : bool =
        let xIntersects = hSegment.Start.X < vSegment.Start.X && hSegment.End.X > vSegment.End.X
        let yIntersects = hSegment.Start.Y > vSegment.Start.Y && hSegment.Start.Y < vSegment.End.Y
        xIntersects && yIntersects

    // Function to determine if a segment is horizontal or vertical based on its index and wire's initial orientation
    let getSegmentOrientation (index: int) (initialOrientation: Orientation) : Orientation =
        match index % 2, initialOrientation with
        | 0, Horizontal | 1, Vertical -> Horizontal
        | _ -> Vertical

    // Iterate over each pair of wires and count intersections
    let mutable count = 0
    wireSegmentsMap 
    |> Map.iter (fun wireId1 segments1 ->
        wireSegmentsMap 
        |> Map.iter (fun wireId2 segments2 ->
            // Ensure wireId1 is less than wireId2 to avoid double counting
            if wireId1 < wireId2 then
                segments1 
                |> List.iteri (fun index1 seg1 ->
                    let orientation1 = getSegmentOrientation index1 (model.Wire.Wires |> Map.find wireId1 |> fun wire -> wire.InitialOrientation)
                    segments2 
                    |> List.iteri (fun index2 seg2 ->
                        let orientation2 = getSegmentOrientation index2 (model.Wire.Wires |> Map.find wireId2 |> fun wire -> wire.InitialOrientation)
                        // Only compare Horizontal with Vertical to avoid redundant comparisons
                        match orientation1, orientation2 with
                        | Horizontal, Vertical -> 
                            if intersects seg1 seg2 then 
                                count <- count + 1
                        | Vertical, Horizontal -> 
                            if intersects seg2 seg1 then 
                                count <- count + 1
                        | _, _ -> () // Ignore other combinations as they're handled in the reverse match
                    )
                )
        )
    )
    count