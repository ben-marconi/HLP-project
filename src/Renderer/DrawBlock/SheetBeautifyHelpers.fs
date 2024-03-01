module SheetBeautifyHelpers

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics
open BlockHelpers
open Symbol //added -check

// Copied for use in helper functions
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
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
///<summary>A lens for reading and writing the dimensions of a custom component symbol.</summary>
///<param name="sym">Symbol to access or modify.</param>

let customComponentDimensions_ = 
    Lens.create
        (fun (sym: Symbol) -> 
            let hS, vS = (Option.defaultValue 1.0 sym.HScale),(Option.defaultValue 1.0 sym.VScale)
            (hS*sym.Component.W, vS*sym.Component.H))
        (fun (newH: float, newW: float) (sym: Symbol) -> setCustomCompHW newH newW sym)
            

// B2
///<summary>Updates the position of a symbol to a specified position.</summary>
///<param name="sym">Symbol to update.</param>
///<returns>A new symbol instance at the updated position.</returns>
let writeSymPosition (sym: Symbol) (newPos: XYPos) : Symbol = 
    {sym with 
        Pos = newPos
        Component = {sym.Component with
                        X = newPos.X
                        Y = newPos.Y}
        LabelBoundingBox = {sym.LabelBoundingBox with  
                                TopLeft = newPos}}


// B3
///<summary>A lens for reading and writing the port order of symbol on a specified edge.</summary>
///<param name="edge">The edge of the symbol with the desired ports.<param>

let PortOrder_ (edge: Edge) = 
    Lens.create
        (fun (sym: Symbol) -> 
                match sym.PortMaps.Order |> Map.tryFind edge with
                | Some ports -> ports
                | None -> [])
        (fun (newOrder: string list) (sym: Symbol) ->
                let updatedOrder = sym.PortMaps.Order |> Map.add edge newOrder
                {sym with PortMaps = {sym.PortMaps with Order = updatedOrder}})
        
// B4
///<summary>A lens for accessing or modifying the state of the inputs of a MUX2</summary>
/// <param name="sym">The MUX2 symbol whose input reversal state is to be accessed or modified.</param>
/// <param name="newState">The new ReversedInputPorts state indicating whether the inputs should be reversed or not.</param>
let reverseMux2InputStates_ = 
    Lens.create
        (fun (sym: Symbol) -> match sym.ReversedInputPorts with | Some state -> state | None -> false )
        (fun (newState: bool) (sym: Symbol) -> {sym with ReversedInputPorts = Some newState} )


// B5
///<summary>Find the position of a port on the sheet</summary>
let portPos (model: SheetT.Model) (portId: string) : XYPos = 
    let symbolModel = model.Wire.Symbol
    getPortLocation None symbolModel portId              


// B6
///<summary>Finds the bounding box of a symbol</summary>
let symbolOutlineBoundingBox (sym: Symbol) : BoundingBox = getSymbolBoundingBox sym


// B7
///<summary>A lens for accessing or modifying the rotation state of a symbol</summary>
let symRotationState_ = 
    Lens.create
        (fun (sym: Symbol) -> sym.STransform.Rotation)
        (fun (rotation: Rotation) (sym: Symbol) -> {sym  with STransform = {sym.STransform with Rotation = rotation}})
          

// B8
///<summary>A lens for accessing or modifying the flip state of a symbol</summary>
let symFlipState_ = 
    Lens.create
        (fun (sym: Symbol) -> sym.STransform.Flipped)
        (fun (flip: bool) (sym: Symbol) -> {sym  with STransform = {sym.STransform with Flipped = flip}})


// T1 
///<summary>Calculates the number of unique pairs of symbols that intersect each other.</summary>
let countIntersectingSymbolPairs (model: SheetT.Model) : int =
    let boundingBoxesWithIndex =
        Map.values model.BoundingBoxes
        |> Seq.toList
        |> List.mapi (fun n bb -> n, bb)
    
    List.allPairs boundingBoxesWithIndex boundingBoxesWithIndex
    |> List.filter (fun ((n1, bb1), (n2, bb2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox bb1 bb2)
    |> List.length

// T2
///<summary>Calculates the total number of distinct visible wire segments that intersect with one or more symbols.</summary>
let countWireSegmentsIntersectingSymbols (model: SheetT.Model) : int =
    let symbolsBoundingBoxes = 
        model.Wire.Symbol.Symbols 
        |> Map.values
        |> Seq.map getSymbolBoundingBox
        |> Seq.toList

    let segmentIntersectsAnySymbol (segmentStart: XYPos) (segmentEnd: XYPos) : bool =
        symbolsBoundingBoxes
        |> List.exists (fun bb -> 
            match segmentIntersectsBoundingBox bb segmentStart segmentEnd with
            | Some _ -> true
            | None -> false)

    let getAbsoluteSegmentPositions (wire: BusWireT.Wire) (model: SheetT.Model) : (XYPos * XYPos) list =
        let startPos = wire.StartPos
        let segmentVectors = visibleSegments wire.WId model
       
        segmentVectors
        |> List.fold (fun (acc, lastEnd: XYPos) vec -> 
            let nextEnd = { X = lastEnd.X + vec.X; Y = lastEnd.Y + vec.Y }
            (acc @ [(lastEnd, nextEnd)], nextEnd)) ([], startPos) 
        |> fst 
    
    let totalIntersections = 
        model.Wire.Wires
        |> Map.fold (fun acc _ wire ->  
            let segmentPositions = getAbsoluteSegmentPositions wire model 
            let intersections = 
                segmentPositions
                |> List.fold (fun count (startPos, endPos) -> 
                    if segmentIntersectsAnySymbol startPos endPos then count + 1 else count) 0
            
            acc + intersections) 0 
    
    let totalWireConnections = (Map.count model.Wire.Wires) * 2

    totalIntersections - totalWireConnections

// T3 
///<summary>Calculates the total number of unique segment pairs within a model that intersect at right angles, excluding segments on the same net.</summary>
let countRightAngleIntersectingSegments (model: SheetT.Model) =

    let segments = 
        model.Wire.Wires 
        |> Map.values 
        |> Seq.collect (fun wire -> getNonZeroAbsSegments wire |> Seq.toList)
        |> List.ofSeq

    let segmentsOnSameNetCheck seg1 seg2 model =
        let wire1 = model.Wire.Wires |> Map.find seg1.WireId
        let wire2 = model.Wire.Wires |> Map.find seg2.WireId
        wire1.OutputPort = wire2.OutputPort

    let segmentsIntersectAtRightAngles (seg1: ASegment) (seg2: ASegment) =
        match seg1.Orientation, seg2.Orientation with
        | Horizontal, Vertical | Vertical, Horizontal ->
            overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X) &&
            overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y) &&
            not (segmentsOnSameNetCheck seg1.Segment seg2.Segment model)
        | _ -> false

    let rightAngleIntersections = 
        List.allPairs segments segments
        |> List.filter (fun (seg1, seg2) -> segmentsIntersectAtRightAngles seg1 seg2)
        |> List.distinct  

    rightAngleIntersections.Length/2

//T4  
///<summary>Calculates the total length of all visible wire segments within the model, counting only one when there are N same-net segments overlapping</summary>
let calcVisibleWireLengths (model: SheetT.Model) : float = 
    let nets = partitionWiresIntoNets model.Wire

    let calculateVectorLength (vector: XYPos) = abs(vector.X) + abs(vector.Y)
    
    let checkNetOverlaps (_, wire)=
            wire
            |> List.collect (fun (wID, _) -> visibleSegments wID model |> List.map calculateVectorLength)
            |> List.sum 

    nets |> List.fold (fun acc net -> acc + (checkNetOverlaps (net))) 0.0



// T5 
///<summary>Calculates the number of visible wire right angles, counting over the whole sheet.</summary>

let countVisibleRightAngles (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.fold (fun acc _ wire ->
        let segments = visibleSegments wire.WId model
        let rightAnglesCount = if List.length segments > 0 then List.length segments-1 else 0 
        acc + rightAnglesCount) 0

