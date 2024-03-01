module SheetBeautifyHelpers

open DrawModelType
open DrawModelType.SymbolT
open CommonTypes
open Optics
open Optics.Optic
open DrawModelType.BusWireT

//-----------------------------------------------------------------------------------------------
// visibleSegments is included here as ahelper for info, and because it is needed in project work
//-----------------------------------------------------------------------------------------------

/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1
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

//B1R
let getCustomSymbolDimension (sym:Symbol) =
    let comp = get component_ sym
    let scale = get scaleF_ sym
    (scale.X * get w_ comp,
     scale.Y * get h_ comp)

//B1W
let setCustomSymbolDimension (width, height) (sym:Symbol) =
    let comp = get component_ sym
    let newComp = set w_ width comp
                |> set h_ height
    set component_ comp sym

/// Returns the dimension of a symbol as a tuple (width * height)
let customSymbolDimension_ = Lens.create (getCustomSymbolDimension) (setCustomSymbolDimension)

//B2W
let setSymbolPosition (position:XYPos) (sym:Symbol) =
    set posOfSym_ position sym
//B3R
let getSymbolPortsOnSide side (sym:Symbol) =
    let pMap = get portMaps_ sym
    get order_ pMap
    |> Map.tryFind side
//B3W
let setSymbolPortsOnSide side ports sym =
    let pMap = get portMaps_ sym
    let orderMap = get order_ pMap
                   |> Map.add side ports
    set portMaps_ (set order_ orderMap pMap) sym

//B4R B4W
let reversedInputPorts_ = Lens.create (fun (a:Symbol) -> a.ReversedInputPorts) (fun s a -> {a with ReversedInputPorts = s})

//B5R
let getPortPosition (sym:Symbol) (port:string) =
    let comp = get component_ sym
    let edge = match Map.tryFind port sym.PortMaps.Orientation with
                | Some x -> x
                | None -> failwithf $"Port with Id '%s{port}' not found on symbol"
    let idx = sym.PortMaps.Order[edge] |> List.findIndex (fun id -> id = port) //Shouldn't fail due to previous check
    let totalPorts = sym.PortMaps.Order[edge] |> List.length
    match edge with
    | Top -> {X = comp.X + (float idx / float (totalPorts+1)) * comp.W; Y = comp.Y}
    | Bottom -> {X = comp.X + (float idx / float (totalPorts+1)) * comp.W; Y = comp.Y - comp.H}
    | Left -> {X = comp.X; Y = comp.Y - (float idx / float (totalPorts+1))}
    | Right -> {X = comp.X + comp.W ; Y = comp.Y - (float idx / float (totalPorts+1))}

//B6R
let getBoundingBoxOfSymbol (model:SheetT.Model) (sym:Symbol) =
    // This exists in Symbol.getSymbolBoundingBox which is probably a better function
    let bb = model.BoundingBoxes
    Map.tryFind sym.Id bb


let stransform_ = Lens.create (fun (a:Symbol) -> a.STransform) (fun s a -> {a with STransform = s})
//B7R
let getSymbolRotation (sym:Symbol) =
    (get stransform_ sym).Rotation
//B7W
let setSymbolRotation (rotation:Rotation) (sym:Symbol) =
    SymbolResizeHelpers.rotateSymbol rotation sym
//B8R
let getSymbolFlip (sym:Symbol) =
    (get stransform_ sym).Flipped
//B8W
let setSymbolFlip (flip:FlipType) (sym:Symbol) =
    SymbolResizeHelpers.flipSymbol flip sym

//T1R
let countSymbolIntersections (sheet: SheetT.Model) =
    let boxes =
        Helpers.mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length

//T2R
let countVisibleWireIntersections (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.filter (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
    |> Map.toList
    |> List.length

let countWireIntersectsWire (w1:Wire) (w2:Wire) =
    BlockHelpers.getAbsSegments w1
    |> List.map (fun aSeg ->
        let folder pos1 pos2 acc seg =
            match BlockHelpers.overlap2D (pos1,pos2) (aSeg.Start,aSeg.End) with
            |true -> acc + 1
            |false -> acc
        BlockHelpers.foldOverNonZeroSegs folder 0 w2)
    |> List.sum

//T3R
/// <summary>
/// counts the number of wire segments that cross each other at right angles across the entire sheet
/// </summary>
let countSegmentsCrossingRightAngle (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    let wires = wireModel.Wires
                |> Map.toList
    List.allPairs wires wires
    //Remove wires paired with themselves
    |> List.filter (fun (w1, w2) -> fst w1 <> fst w2)
    //Filter wires that start from the same output port
    |> List.map (fun (w1, w2) -> snd w1, snd w2)
    |> List.filter (fun (w1, w2) -> w1.OutputPort <> w2.OutputPort)
    // Count the number of intersections between each wire pair
    |> List.map (fun (w1,w2) -> countWireIntersectsWire w1 w2)
    |> List.sum

//T4R
/// <summary>
/// Calculate the length of the visible wires on the sheet, excluding wires that overlap from the same net
/// </summary>
let visibleWireLength (sheet:SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.map (fun k _ -> visibleSegments k sheet)
    |> Map.map (fun k segs ->
        segs
        |> List.fold (fun (acc,lastPos) seg ->
                      if acc = 0. then (0., seg)
                      elif seg.X = lastPos.X then (acc+abs(seg.Y - lastPos.Y), seg)
                      elif seg.Y = lastPos.Y then (acc+abs(seg.X - lastPos.X), seg)
                      else (acc, seg)
                      ) (0.,XYPos.zero)
        )
    |> Map.map (fun k (acc,_) -> acc)
    |> Map.toList
    |> List.sumBy snd

//T5R
/// <summary>
/// Number of visible right angles over whole sheet
/// </summary>
let countVisibleRightAngles (sheet:SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.map (fun k _ -> visibleSegments k sheet)
    |> Map.map (fun k segs ->
        segs
        |> List.fold (fun (acc,lastPos) seg ->
                      if acc = 0 then (0, seg)
                      elif seg.X = lastPos.X then (acc, seg)
                      elif seg.Y = lastPos.Y then (acc, seg)
                      else (acc+1, seg)
                      ) (0,XYPos.zero)
        )
    |> Map.map (fun k (acc,_) -> acc)
    |> Map.toList
    |> List.sumBy snd

//T6R
/// <summary>
/// Find all segments that retrace back over the wire they came from and the end segments that retrace so far the next seg is inside a sym
/// </summary>
/// <param name="sheet"></param>
/// <returns> Fst is segments that retrace, snd are end segments that retrace so far the next seg is inside a sym</returns>
// let retracingWireSegments (sheet:SheetT.Model):(ASegment list * ASegment list) =
//     let wires = sheet.Wire.Wires
//                 |> Map.map (fun k v -> BlockHelpers.getAbsSegments v)



let countWireLabelIntersections (sheet:SheetT.Model) =
    let wires = sheet.Wire.Wires |> Map.toList
    let labels = sheet.Wire.Symbol.Symbols
                 |> Map.filter (fun _ sym -> sym.Component.Type = IOLabel)
    let labelBoxes = labels
                     |> Map.map (fun _ sym -> getBoundingBoxOfSymbol sheet sym)
                     |> Map.toList
                     |> List.choose (fun (_, x) -> x)
    List.allPairs wires labelBoxes
    |> List.map (fun (w,bbox) ->
            let folder pos1 pos2 acc seg =
                match BlockHelpers.overlap2D (pos1,pos2) (bbox.TopLeft,bbox.BottomRight()) with
                | true -> acc + 1
                | false -> acc
            BlockHelpers.foldOverNonZeroSegs folder 0 (snd w))
    |> List.sum

