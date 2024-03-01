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
open Optics.Optic
open BlockHelpers


/// <summary>
/// B1R
/// Reads the dimensions of a custom symbol, applying scaling factors if available.
/// </summary>
/// <param name="symbol">The symbol whose dimensions are to be read.</param>
/// <returns>
/// A tuple containing the width and height of the symbol after applying scaling factors.
/// </returns>
let readCustomSymbolDimensions (symbol: Symbol) : (float * float) = 
    let hScale = Option.defaultValue 1.0 symbol.HScale
    let vScale = Option.defaultValue 1.0 symbol.VScale
    let h = symbol.Component.H
    let w = symbol.Component.W
    let getWidth =  hScale * w
    let getHeight = vScale * h
    getWidth, getHeight


/// <summary>
/// B1W
/// Writes new dimensions to a custom symbol, updating the width and height.
/// </summary>
/// <param name="newW">The new width to be set.</param>
/// <param name="newH">The new height to be set.</param>
/// <param name="symbol">The symbol to be updated.</param>
/// <returns>The updated symbol with the new dimensions.</returns>
let writeCustomSymbolDimensions (newW: float, newH: float) (symbol: Symbol) : (Symbol) =
    let getComponent = get component_ symbol
    let updatedComponent = set w_ newW getComponent
                        |> set h_ newH
    set component_ updatedComponent symbol

/// <summary>
/// B1
/// Lens for accessing and modifying the dimensions of a custom symbol.
/// </summary>
let customSymbolDimensions_ = Lens.create 
                                (readCustomSymbolDimensions) (writeCustomSymbolDimensions)

/// <summary>
/// B2
/// Changes the position of a symbol to the specified coordinates.
/// </summary>
/// <param name="symbol">The symbol whose position is to be changed.</param>
/// <param name="newPosition">The new coordinates for the symbol's position.</param>
/// <returns>The updated symbol with the new position.</returns>
let changeSymbolPosition (symbol: Symbol) (newPosition: XYPos) = 
    set posOfSym_ newPosition symbol

/// <summary>
/// B3R
/// Reads the order of ports on a specified side edge of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose port order is to be read.</param>
/// <param name="sideEdge">The side edge for which the port order is requested.</param>
/// <returns>
/// The order of ports on the specified side edge. Returns an empty list if the side edge is not found.
/// </returns>
let readPortOrder(symbol: Symbol) (sideEdge: Edge) = 
    let portMap = get portMaps_ symbol
    get order_ portMap
    |> Map.tryFind sideEdge
    |> Option.defaultValue []

/// <summary>
/// B3W
/// Writes the order of ports on a specified side edge of a symbol.
/// </summary>
/// <param name="symbol">The symbol whose port order is to be modified.</param>
/// <param name="portOrderList">The new order of ports on the specified side edge.</param>
/// <param name="sideEdge">The side edge for which the port order is modified.</param>
/// <returns>The updated symbol with the modified port order.</returns>
let writePortOrder (symbol: Symbol) (portOrderList: string list) (sideEdge: Edge) =
    let portMap = get portMaps_ symbol
    let newPortMap = get order_ portMap
                    |> Map.add sideEdge portOrderList
    set portMaps_ (set order_ newPortMap portMap) symbol

                                            


/// <summary>
/// B4
/// Lens for reading and writing the reversed input ports property of a symbol.
/// </summary>
/// <param name="symbol">The symbol to read from or write to.</param>
/// <returns>The value indicating whether the input ports are reversed.</returns>
let reverseInputPort_ = Lens.create (fun(symbol: Symbol) ->
                                            match symbol.ReversedInputPorts with
                                            | Some reversed -> reversed
                                            | None -> failwithf "Symbol %s does not have reversed input ports" symbol.Component.Label)
                                    (fun(reversed: bool) (symbol: Symbol) ->
                                            {symbol with ReversedInputPorts = Some reversed}
                                            )
                                        


/// <summary>
/// B5
/// Retrieves the position of a port within the sheet model.
/// </summary>
/// <param name="model">The sheet model containing the port.</param>
/// <param name="portId">The identifier of the port to retrieve the position for.</param>
/// <returns>The position (location) of the specified port within the sheet model.</returns>
let positionPortSheet (model: SheetT.Model) (portId: string) = 
    getPortLocation None model.Wire.Symbol portId                                         


/// <summary>
/// B6
/// Retrieves the bounding box of a symbol within the sheet model.
/// </summary>
/// <param name="model">The sheet model containing the symbol.</param>
/// <param name="symbol">The symbol for which to retrieve the bounding box.</param>
/// <returns>
/// The bounding box of the specified symbol within the sheet model, or None if the symbol is not found.
/// </returns>
let getSymbolBoundingBox (model: SheetT.Model) (symbol:Symbol) = 
    Map.tryFind symbol.Id model.BoundingBoxes

/// <summary>
/// B7
/// Lens for accessing or modifying the rotation state of a symbol.
/// </summary>

let rotationStateSymbol_ = Lens.create (fun(symbol: Symbol) -> symbol.STransform.Rotation)
                                    (fun(newRotation: Rotation) (symbol: Symbol) ->
                                        {symbol with STransform = ({symbol.STransform with Rotation = newRotation})})

/// <summary>
/// B8
/// Lens for accessing or modifying the flip state of a symbol.
/// </summary>
let flipStateSymbol_ = Lens.create (fun(symbol: Symbol) -> symbol.STransform.Flipped)
                                    (fun(newFlipped: bool) (symbol: Symbol) ->
                                        {symbol with STransform = ({symbol.STransform with Flipped = newFlipped})})

/// <summary>
/// Helper Funciton: Retrieves all symbol bounding boxes from the provided sheet model.
/// </summary>
/// <param name="model">The sheet model containing symbol bounding boxes.</param>
/// <returns>
/// A list of tuples where each tuple consists of a symbol identifier and its corresponding bounding box.
/// </returns>
let allSymbolBoundingBoxes (model: SheetT.Model) = 
    Map.values model.BoundingBoxes
    |> List.ofSeq
    |> List.mapi (fun id bb -> id, bb)

/// <summary>
/// T1
/// Counts the number of intersecting distinct pairs of symbols in the provided sheet model.
/// </summary>
/// <param name="model">The sheet model containing symbols and their bounding boxes.</param>
/// <returns>The count of intersecting distinct symbol pairs.</returns>
let intersectingDistinctSymbolPair (model: SheetT.Model): int =
    let symbolsBB = allSymbolBoundingBoxes model

    let isDifferentPair ((n1: int,_),(n2: int,_)) = n1 < n2
    let overlapBB ((_,bb1: BoundingBox), (_, bb2: BoundingBox)) = 
        overlap2DBox bb1 bb2
    
    List.allPairs symbolsBB symbolsBB
    |> List.filter isDifferentPair
    |> List.filter overlapBB
    |> List.length


/// <summary>
/// Helper Function from TestDrawBlock.fs
/// Returns a list of XY vectors representing visible segments of a wire based on its initial orientation.
/// </summary>
/// <param name="wId">ConnectionId of the wire in the model.</param>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>List of XY vectors representing visible segments of the wire.</returns>
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: BusWireT.Segment) : XYPos =
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

    
/// <summary>
/// T3
/// Counts the number of right-angle intersections between segments of different wires in the sheet model.
/// </summary>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>The count of right-angle intersections between segments of different wires.</returns>
let segmentPairRightAngleCount (model: SheetT.Model) : int =
    // Extract the wires from the model with their indices
    let wires = Map.values model.Wire.Wires
                |> List.ofSeq
                |> List.mapi (fun id wire -> id, wire)
    
    // Helper function to get pairs of segments from two wires
    let getSegmentPairs w1 w2 = 
        let segs1 = BlockHelpers.getNonZeroAbsSegments w1
        let segs2 = BlockHelpers.getNonZeroAbsSegments w2
        List.allPairs segs1 segs2

    // Check if two segments form a right-angle pair
    let isRightAnglePair (seg1: ASegment) (seg2: ASegment) = 
        match seg1.Orientation, seg2.Orientation with
        | Horizontal, Vertical | Vertical, Horizontal -> true
        | _ -> false
    
    // Check if two segments intersect
    let segmentIntersect seg1 seg2 =
        BlockHelpers.overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X) &&
        BlockHelpers.overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)

    // Check if two wires belong to different nets
    let differentNet (w1: Wire) (w2: Wire) = 
        w1.OutputPort <> w2.OutputPort
        
    // Check if two indices form a different pair
    let isDifferentPair ((n1: int,_),(n2: int,_)) = n1 < n2

    // Get all pairs of wires
    List.allPairs wires wires
    |> List.filter isDifferentPair
    |> List.filter (fun (w1, w2) -> differentNet (snd w1) (snd w2))
    |> List.map (fun (w1, w2) -> getSegmentPairs (snd w1) (snd w2))
    |> List.concat
    |> List.filter (fun (seg1, seg2) -> isRightAnglePair seg1 seg2 && segmentIntersect seg1 seg2)
    |> List.distinct
    |> List.length



/// <summary>
/// T5
/// Counts the number of right-angle intersections between visible segments of different wires in the sheet model.
/// </summary>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>The count of right-angle intersections between visible segments of different wires.</returns>
let visibleWireRightAngleCount (model: SheetT.Model) : int = 
    // Get the IDs of all wires in the model
    let wiresIds = Map.keys model.Wire.Wires |> List.ofSeq
    
    // Calculate the total count of right-angle intersections for visible segments
    let rightAngleCount =
        wiresIds
        |> List.fold (fun acc wId -> acc + List.length (visibleSegments wId model) - 1) 0

    rightAngleCount




/// <summary>
/// T6
/// Finds segments in the model that have zero length and are part of retracing patterns.
/// </summary>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>List of segments forming retracing patterns with zero length.</returns>
let retracingSegments (model: SheetT.Model) : ASegment list =
    // Get all wires from the model
    let wires = Map.values model.Wire.Wires |> List.ofSeq
    // Get all absolute segments from all wires
    let getAllSegments = List.collect (fun wire -> BlockHelpers.getAbsSegments wire) wires

    /// Helper function to determine retracing patterns with zero length
    let returnRetracingIndexes (segs1, segs2, segs3) =
        match segs2.Segment.Length with
        | 0.0 ->
            match segs1.Segment.Length, segs3.Segment.Length with
            | lenPrev, lenNext when lenPrev < 0.0 && lenNext < 0.0 -> [segs1; segs2; segs3]
            | lenPrev, lenNext when lenPrev < 0.0 && lenNext > 0.0 -> [segs1; segs2]
            | lenPrev, lenNext when lenPrev > 0.0 && lenNext < 0.0 -> [segs2; segs3]
            | _ -> []
        | _ -> []

    // Find and collect segments forming retracing patterns with zero length
    let zeroLengthSegmentsWithRetracing =
        getAllSegments
        |> List.windowed 3
        |> List.collect (fun segs -> 
            returnRetracingIndexes (segs.[0], segs.[1], segs.[2])
        )

    zeroLengthSegmentsWithRetracing


/// <summary>
/// Counts the number of wires intersecting with visible symbols on the sheet.
/// </summary>
/// <param name="model">The sheet model.</param>
/// <returns>The count of wires intersecting with visible symbols.</returns>
let intersectVisibleWireWithSymbolCount (model: SheetT.Model) : int =
    let boundinBoxes = Map.values model.BoundingBoxes
                        |> List.ofSeq  
    List.collect (fun bb -> getWiresInBox bb model.Wire) boundinBoxes
    |> List.length         


/// <summary>
/// T4
/// Calculates the total length of visible segments in the sheet model.
/// </summary>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>Total length of visible segments.</returns>
let visibleSegmentLength (model: SheetT.Model) : float = 
    let wires = Map.keys model.Wire.Wires
                |> List.ofSeq
                
    let visibleWiresLength = List.collect (fun wId -> visibleSegments wId model) wires
                                    |> List.fold (fun total_len len -> total_len + abs len.X + abs len.Y) 0.0
    visibleWiresLength
    

