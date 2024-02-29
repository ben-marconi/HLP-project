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




//B1R, B1W - Read and Write the dimensions of a custom component symbol
// Partially Complete
//READ EDSTEM -> SAYS VALUE WRITTEN WILL BE HEIGHT AND WIDTH HERE - but its a custom component symbol and symbol includes scaling?
let customComponentDimensions_ = 
    Lens.create
        (fun (sym: Symbol) -> 
            let hS, vS = (Option.defaultValue 1.0 sym.HScale),(Option.defaultValue 1.0 sym.VScale)
            (hS*sym.Component.W, vS*sym.Component.H))
        (fun (newHeight: float, newWidth: float) (sym: Symbol) -> setCustomCompHW newHeight newWidth sym)
            //{sym with HScale = Some (newHeight/sym.Component.H); VScale = Some (newWidth/sym.Component.W)})
//Should it be hscale input or height and converted as i did 

//B2W - Write the position of a symbol on the sheet
let writePosition (sym: Symbol) (newPos: XYPos) = {sym with Pos = newPos}
// Change


let WritePosition2 (model: Model) (symId: ComponentId) (newPos: XYPos) : Model = 
    let symbol = 
        match Map.tryFind symId model.Wire.Symbol.Symbols with
        | Some symbol -> symbol
        | None -> failwithf "Symbol does not exist in the model"

    let updatedSymbol = {symbol with Pos = newPos}
    let updatedBoundingBox = getSymbolBoundingBox updatedSymbol     // Is this function for the right sort of symbol?

    let updatedSymbols = model.Wire.Symbol.Symbols |> Map.add symId updatedSymbol
    let updatedBoundingBoxes = model.BoundingBoxes |> Map.add symId updatedBoundingBox

    {model with Wire = {model.Wire with Symbol = {model.Wire.Symbol with Symbols = updatedSymbols}}; BoundingBoxes = updatedBoundingBoxes}
// Look at movesymbols in symbolUpdate




// Look at updateBoundingBoxes functions -> end of sheetupdatehelpers.fs
// Do we need to do snaps like they do in SheetUpdateHelpers.fs moveSymbols function
//getSympolPos in BlockHelpers.fs?
//posOfSym_


//B3R, B3W - Read and Write the order of ports on a specified side of the symbol

let adjustPortOrder_ (edge: Edge) = 
    Lens.create
        (fun (sym: Symbol) -> 
                match sym.PortMaps.Order |> Map.tryFind edge with
                | Some ports -> ports
                | None -> [])
        (fun (newOrder: string list) (sym: Symbol) ->
                let updatedOrder = sym.PortMaps.Order |> Map.add edge newOrder
                {sym with PortMaps = {sym.PortMaps with Order = updatedOrder}})
        
//B4R, B4W - Reverses the state of inputs of a MUX2
let reverseInputStates_ = 
    Lens.create
        (fun (sym: Symbol) -> match sym.ReversedInputPorts with | Some state -> state | None -> false )
        (fun (newState: bool) (sym: Symbol) -> {sym with ReversedInputPorts = Some newState} )


//B5R - The position of a port on the sheet. It can not be directly written.

let portPos (model: SheetT.Model) (portId: string) : XYPos = 
    let symbolModel = model.Wire.Symbol
    getPortLocation None symbolModel portId              

//wireSymEdge in BlockHelpers.fs?
//getPort in BlockHelpers.fs?
// GETpORTpOS  - TOP LEFT OF SO symbol.pos + getportpose symbol port


//B6R - The Bounding box of a symbol outline (position is contained in this)
let symbolOutlineBoundingBox (sym: Symbol) : BoundingBox = getSymbolBoundingBox sym


//B7R, B7W - The rotation state of a symbol
let symbolRotationState_ = 
    Lens.create
        (fun (sym: Symbol) -> sym.STransform.Rotation)
        (fun (rotation: Rotation) (sym: Symbol) -> {sym  with STransform = {sym.STransform with Rotation = rotation}})
          

//B8R, B8W - The flip state of a symbol

let symbolFlipState_ = 
    Lens.create
        (fun (sym: Symbol) -> sym.STransform.Flipped)
        (fun (flip: bool) (sym: Symbol) -> {sym  with STransform = {sym.STransform with Flipped = flip}})

// T1 - Read the number of pairs of symbols that intersect each other. Count over all pairs of symbols

let countIntersectingSymbolPairs (sheet: SheetT.Model) : int =
    let boundingBoxesWithIndex =
        Map.values sheet.BoundingBoxes
        |> Seq.toList
        |> List.mapi (fun n bb -> n, bb)
    
    List.allPairs boundingBoxesWithIndex boundingBoxesWithIndex
    |> List.filter (fun ((n1, bb1), (n2, bb2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox bb1 bb2)
    |> List.length

// T2 - Read the number of distinct wire visible segments that intersect with one or more symbols. Counter over all visible wire segments

// let countWireSegmentsIntersectingSymbols (model: SheetT.Model) : int =
    
//     let symbolsBoundingBoxes = 
//         model.Wire.Symbol.Symbols 
//         |> Map.values
//         |> Seq.map getSymbolBoundingBox
//         |> Seq.toList

//     let segmentIntersectsAnySymbol (segmentStart: XYPos) (segmentEnd: XYPos) : bool =
//         symbolsBoundingBoxes
//         |> List.exists (fun bb -> 
//             match BlockHelpers.segmentIntersectsBoundingBox bb segmentStart segmentEnd with
//             | Some _ -> true
//             | None -> false)

//     // NEEDS REVIEWING
//     let getAbsoluteSegmentPositions (wire: BusWireT.Wire) (model: SheetT.Model) : (XYPos * XYPos) list =
//         let startPos = wire.StartPos
//         let segmentVectors = visibleSegments wire.WId model
       
//         segmentVectors
//         |> List.fold (fun (acc, lastEnd) vec -> 
//             let nextEnd = { X = lastEnd.X + vec.X; Y = lastEnd.Y + vec.Y }
//             (acc @ [(lastEnd, nextEnd)], nextEnd)) ([], startPos) 
//         |> fst 
    
//     model.Wire.Wires
//     |> Map.fold (fun acc _ wire ->  
//         let segmentPositions = getAbsoluteSegmentPositions wire model 
//         let intersections = 
//             segmentPositions
//             |> List.pairwise
//             |> List.fold (fun count (startPos, endPos) -> 
//                 if segmentIntersectsAnySymbol startPos endPos then count + 1 else count) 0
        
//         acc + intersections) 0 


//getwiresandbox
    
// T3 - Read the number of distinct pairs of segments that cross each other at right angles
// Does not include 0 length segments or segments on the same net intersecting at one end, or segements on top of each other

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

    rightAngleIntersections.Length

// T4



// T5 -  Read the number of visible wire right-angles. Count over the whole sheet

let countVisibleRightAngles (model: SheetT.Model) =
    model.Wire.Wires
    |> Map.fold (fun acc _ wire ->
        let segments = visibleSegments wire.WId model
        let rightAnglesCount = if List.length segments > 0 then List.length segments-1 else 0 
        acc + rightAnglesCount) 0
