module SheetBeautifyHelpers

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Symbol
open Optics

// Function to read or write dimensions of a custom component symbol

let readCustomSymbolDimensions (symbol: Symbol) = 
    let hScale = Option.defaultValue 1.0 symbol.HScale
    let vScale = Option.defaultValue 1.0 symbol.VScale
    let h = symbol.Component.H
    let w = symbol.Component.W
    let getWidth =  hScale * w
    let getHeight = vScale * h
    getWidth, getHeight

let writeCustomSymbolDimensions (newW: float, newH: float) (symbol: Symbol) =
    let getComponent = fst component_ symbol
    let updatedComponent = snd w_ newW getComponent
                        |> snd h_ newH
    snd component_ updatedComponent symbol
//B1
let customSymbolDimensions = Lens.create 
                                (readCustomSymbolDimensions) (writeCustomSymbolDimensions)

//B2
let changeSymbolPosition (symbol: Symbol) (newPosition: XYPos) = 
    snd posOfSym_ newPosition symbol

//B3
let getPortOrder (symbol: SymbolT.Symbol) () = "Not implemented yet"
                                                
    


//B4
let reverseInputPort_ = Lens.create (fun(symbol: Symbol) ->
                                            match symbol.ReversedInputPorts with
                                            | Some reversed -> reversed
                                            | None -> failwithf "Symbol %s does not have reversed input ports" symbol.Component.Label)
                                    (fun(reversed: bool) (symbol: Symbol) ->
                                            {symbol with ReversedInputPorts = Some reversed}
                                            )
                                        

//B5
let positionPortSheet (model: SheetT.Model) (portId: string) = 
    getPortLocation None model.Wire.Symbol portId                                         


//B6
let boundingBox (model: SheetT.Model) (symbol:Symbol) = 
    Map.tryFind symbol.Id model.BoundingBoxes

//B7

let rotationStateSymbol_ = Lens.create (fun(symbol: Symbol) -> symbol.STransform.Rotation)
                                    (fun(newRotation: Rotation) (symbol: Symbol) ->
                                        {symbol with STransform = ({symbol.STransform with Rotation = newRotation})})

//B8
let flipStateSymbol_ = Lens.create (fun(symbol: Symbol) -> symbol.STransform.Flipped)
                                    (fun(newFlipped: bool) (symbol: Symbol) ->
                                        {symbol with STransform = ({symbol.STransform with Flipped = newFlipped})})


let allSymbolBoundingBoxes (model: SheetT.Model) = 
    Map.values model.BoundingBoxes
    |> List.ofSeq
    |> List.mapi (fun id bb -> id, bb)

//T1
let intersectingDistinctSymbolPair (model: SheetT.Model): int =
    let symbolsBB = allSymbolBoundingBoxes model

    let isDifferentPair ((n1: int,_),(n2: int,_)) = n1 < n2
    let overlapBB ((_,bb1: BoundingBox), (_, bb2: BoundingBox)) = 
        BlockHelpers.overlap2DBox bb1 bb2
    
    List.allPairs symbolsBB symbolsBB
    |> List.filter isDifferentPair
    |> List.filter overlapBB
    |> List.length



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

    /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// wherever this is possible
    let rec coalesce (segVecs: XYPos list)  =
        match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
        | Some zeroVecIndex ->
            let index = zeroVecIndex + 1 // base index as it should be on full segVecs
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
            |> coalesce
        | None -> segVecs
    
    wire.Segments
    |> List.mapi getSegmentVector
    |> coalesce

// let visibleSegments (wId: ConnectionId) (model: SheetT.Model): (XYPos * XYPos) list =

//     let wire = model.Wire.Wires[wId] // get wire from model

//     /// helper to match even and odd integers in patterns (active pattern)
//     let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

//     /// Convert seg into its start and end points.
//     /// index must be the index of seg in its containing wire.
//     let getSegmentEndpoints (index:int) (seg: BusWireT.Segment) =
//         let start = match index, wire.InitialOrientation with
//                     | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> seg.Start
//                     | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> seg.Start + {X=seg.Length; Y=0.}
//         let endPos = start + 
//                         match index, wire.InitialOrientation with
//                         | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
//                         | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}
//         (start, endPos)

//     /// Return the list of segment endpoints with consecutive zero vectors coalesced
//     let rec coalesce (segEndpoints: (XYPos * XYPos) list)  =
//         match List.tryFindIndex (fun (_, endPos) -> endPos =~ XYPos.zero) segEndpoints[1..segEndpoints.Length-2] with          
//         | Some zeroEndPosIndex ->
//             let index = zeroEndPosIndex + 1 // base index as it should be on full segEndpoints
//             segEndpoints[0..index-2] @
//             [(fst (List.item (index-1) segEndpoints), snd (List.item (index+1) segEndpoints))] @
//             segEndpoints[index+2..segEndpoints.Length - 1]
//             |> coalesce
//         | None -> segEndpoints
    
//     wire.Segments
//     |> List.mapi getSegmentEndpoints
//     |> coalesce



let getVisibleSegments (wiresModel: BusWireT.Model) = 
    let getWires = wiresModel.Wires

    let getAllSegements : ASegment list = 
        Map.values getWires
        |> List.ofSeq
        |> List.map (fun wire -> BlockHelpers.getAbsSegments wire)
        |> List.concat

    List.mapi (fun i seg -> i, seg) getAllSegements




//T2
// let intersectingDistinctWiresandSymbols (model: SheetT.Model) = 
//     let symbolsBB = allSymbolBoundingBoxes model |> List.map snd
//     // symbolsBB
//     // let wires = model.Wire
//     // let a = List.map (fun bb ->BlockHelpers.getWiresInBox bb wires) symbolsBB
//     // List.concat a
//     let wIds = model.Wire.Wires |> Map.keys
//     List.ofSeq wIds
//     |> List.map (fun wId -> visibleSegments wId model)
//     |> List.concat

    
//T3
let segmentPairRightAngleCount (model: SheetT.Model) =
    let wires = Map.values model.Wire.Wires
                |> List.ofSeq
                |> List.mapi (fun id wire -> id, wire)
    
    let getSegmentPairs w1 w2 = 
        let segs1 = BlockHelpers.getNonZeroAbsSegments w1
        let segs2 = BlockHelpers.getNonZeroAbsSegments w2
        List.allPairs segs1 segs2

    let isRightAnglePair (seg1: ASegment) (seg2:ASegment) = 
        match seg1.Orientation, seg2.Orientation with
        | Horizontal, Vertical | Vertical, Horizontal -> true
        | _ -> false
    
    let segmentIntersect seg1 seg2 =
        BlockHelpers.overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X) &&
        BlockHelpers.overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)

    let differentNet (w1:Wire) (w2:Wire) = 
        w1.OutputPort <> w2.OutputPort
        
    let isDifferentPair ((n1: int,_),(n2: int,_)) = n1 < n2

    List.allPairs wires wires
    |> List.filter isDifferentPair
    |> List.filter (fun (w1, w2) -> differentNet (snd w1) (snd w2))
    |> List.map (fun (w1, w2) -> getSegmentPairs (snd w1) (snd w2))
    |> List.concat
    |> List.filter (fun (seg1, seg2) -> isRightAnglePair seg1 seg2 && segmentIntersect seg1 seg2)
    |> List.distinct
    |> List.length
    
//T5
let visibleWireRightAngleCount (model:SheetT.Model) = 
    let wiresIds = Map.keys model.Wire.Wires |> List.ofSeq
    
    let RightAngleCount =
        wiresIds
        |> List.fold (fun acc wId -> acc + List.length (visibleSegments wId model) - 1) 0

    RightAngleCount


//T4

let visibleSegmentLength (model: SheetT.Model) = 
    let wires = Map.keys model.Wire.Wires
                |> List.ofSeq
                
    let visibleWires = List.map (fun wId -> visibleSegments wId model) wires
                                |> List.concat
    visibleWires
    |> List.fold (fun total_len len -> total_len + abs len.X + abs len.Y) 0.0



//T6
let retracingSegments (model: SheetT.Model) =
    let wires = Map.values model.Wire.Wires |> List.ofSeq
    let getAllSegments = List.collect (fun wire -> BlockHelpers.getAbsSegments wire) wires
    let returnRetracingIndexes (segs1, segs2, segs3) =
        match segs2.Segment.Length with
        | 0.0 ->
            match segs1.Segment.Length, segs3.Segment.Length with
            | lenPrev, lenNext when lenPrev < 0.0 && lenNext < 0.0 -> [segs1; segs2; segs3]
            | lenPrev, lenNext when lenPrev < 0.0 && lenNext > 0.0 -> [segs1; segs2]
            | lenPrev, lenNext when lenPrev > 0.0 && lenNext < 0.0 -> [segs2; segs3]
            | _ -> []
        | _ -> []
    
    let zeroLengthSegmentsWithRetracing =
        getAllSegments
        |> List.windowed 3
        |> List.collect (fun segs -> 
            returnRetracingIndexes (segs.[0], segs.[1], segs.[2])
        )
    
    zeroLengthSegmentsWithRetracing
