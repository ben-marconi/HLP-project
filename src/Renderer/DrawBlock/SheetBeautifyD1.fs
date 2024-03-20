module SheetBeautifyD1
// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SymbolUpdate
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open BlockHelpers
open SegmentHelpers

/// <summary>
/// Filters parallel wires from the model based on the number of visible segments.
/// </summary>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>List of wires with exactly three visible segments.</returns>
let parallelWires (model: SheetT.Model) =
    Map.values model.Wire.Wires
    |> List.ofSeq
    |> List.filter (fun wire -> List.length (visibleSegments wire.WId model) >= 3)

/// <summary>
/// Gets all single-connected symbols associated with a list of wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <param name="wires">List of wires to consider for symbol connections.</param>
/// <returns>List of single-connected symbols.</returns>
let getAllSingleConnectedSymbols (model: SheetT.Model) (wires: Wire list) =
    let sourceTargetSymbols =
        List.map (fun wire -> (getSourceSymbol model.Wire wire, getTargetSymbol model.Wire wire, wire)) wires

    let getSingleConnectedSymbols (symbolList: (Symbol * Symbol * Wire) list) =
        symbolList
        |> List.groupBy (fun sym -> sym)
        |> List.filter (fun (_, symList) -> List.length symList = 1)
        |> List.map (fun (sym, _) -> sym)
        // |> (fun symList -> 
        //     match symList with
        //     | [] -> []
        //     | (s1,t1,w1)::(s2,t2,w2)::rest -> (s1,t1,w1, t1=s1)

        // )
    let singleConnected = getSingleConnectedSymbols sourceTargetSymbols
    let sourceSymbols = List.map (fun (s, _, _) -> s.Id) singleConnected
    let targetSymbols = List.map (fun (_, t, _) -> t.Id) singleConnected
    
    let moveSource sl tl s t=
        (not (List.contains s tl) || List.contains t sl)


    singleConnected
    |> List.map (fun (s, t, w) -> (s, t, w, moveSource sourceSymbols targetSymbols s.Id t.Id))




    // getSingleConnectedSymbols sourceTargetSymbols
    // |> moveSourceSymbol

/// <summary>
/// Gets all symbols associated with parallel wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <returns>List of symbols connected by parallel wires.</returns>
let getAllSymbols (model: SheetT.Model) =
    getAllSingleConnectedSymbols model (parallelWires model)

/// <summary>
/// Gets the bounding boxes of symbols associated with parallel wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <returns>List of bounding boxes of symbols connected by parallel wires.</returns>
let getSymbolsBoundingBox (model: SheetT.Model) =
    let symbols = getAllSymbols model
    symbols
    |> List.map (fun (symS, symT, _, _) -> getSymBoundingBox symS)

/// <summary>
/// Check for overlaps between symbols after they have been moved.
/// </summary>
/// <param name="symbols">The symbol that checks for overlapping.</param>
/// <returns>List of overlaps</returns>
let detectOverlaps (symbols: Symbol list) : (Symbol * Symbol) list =
    let boundingBoxes = symbols |> List.map (fun sym -> (sym, getSymBoundingBox sym))
    let overlaps =
        boundingBoxes
        |> List.collect (fun (sym1, box1) ->
            boundingBoxes
            |> List.filter (fun (sym2, box2) ->
                sym1 <> sym2 && overlap2DBox box1 box2)
            |> List.map (fun (sym2, _) -> (sym1, sym2)))
    overlaps

let resolveOverlaps (overlaps: (Symbol * Symbol) list) : Symbol list -> Symbol list =
    let moveApart (sym1: Symbol, sym2: Symbol) : Symbol * Symbol =
        // Calculate current bounding boxes
        let bb1 = getSymBoundingBox sym1
        let bb2 = getSymBoundingBox sym2

        // Determine the overlap along X and Y
        let overlapX = max 0.0 (bb1.TopLeft.X + bb1.W - bb2.TopLeft.X)
        let overlapY = max 0.0 (bb1.TopLeft.Y + bb1.H - bb2.TopLeft.Y)

        // Determine how much to move the symbols to no longer overlap
        let moveDistanceX = if overlapX > 0.0 then overlapX + 1.0 else 0.0
        let moveDistanceY = if overlapY > 0.0 then overlapY + 1.0 else 0.0

        // Check the direction of movement needed based on your layout preference
        // This example assumes moving sym2 to the right and/or down
        let newPosX = sym2.Pos.X + moveDistanceX
        let newPosY = sym2.Pos.Y + moveDistanceY

        // Move sym2 by updating its position
        let sym2Moved = { sym2 with Pos = { X = newPosX; Y = newPosY } }

        // Return the updated symbols
        (sym1, sym2Moved)

    let updatedSymbols (symbols) = 
        overlaps |> List.fold (fun acc (sym1, sym2) ->
            let (movedSym1, movedSym2) = moveApart(sym1, sym2) 
            // Update the list of symbols with the new positions
            // Ensure that the list remains unique and update only the moved symbols
            acc |> List.map (fun sym ->
                if sym.Id = movedSym1.Id then movedSym1
                elif sym.Id = movedSym2.Id then movedSym2
                else sym)
        ) symbols  // Start with the original list of symbols
    updatedSymbols  // Return the updated list of symbols
    





/// <summary>
/// Aligns all singly connected symbols by their target port.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols</param>
/// <returns>Updated model with the moved symbols</returns>
let alignSingleConnectedSyms (model: SheetT.Model) (syms) =
    // let syms = getAllSingleConnectedSymbols model (parallelWires model)
                //Prefer moving symbols with only one port to have minimal impact on the rest of the sheet
                //Need to modify the delta function to account for different directions when moving source/target
                // |> List.map (fun (s,t,w) ->
                //                 printfn $"{s.PortMaps.Order.Count}, {t.PortMaps.Order.Count}"
                //                 match (s.PortMaps.Order.Count, t.PortMaps.Order.Count) with
                //                 | (1,1) -> (s,w)
                //                 | (_, 1) -> (t,w)
                //                 | (_, _) -> (s,w)
                //             )
    let checkXCoor (s1: Symbol) (s2: Symbol) (b: bool) =
        // printfn "Distance between %s and %s: %f" s1.Component.Label s2.Component.Label (abs(s1.Pos.X - s2.Pos.X))
        let sub = if b then 1 else -1
        if abs(s1.Pos.X - s2.Pos.X) < 60 then
            (-100*sub)
        else 0
    let delta (wire:Wire) (b:bool) (x: int) =
        let sub = if b then -1. else 1.
        match wire.InitialOrientation with
        | Horizontal -> {X=x; Y=wire.EndPos.Y - (wire.StartPos.Y)}
        | Vertical -> {X=wire.EndPos.X - (wire.StartPos.X); Y=0}

    let symbolMap = Optic.get symbols_ model
    let movedSyms = List.map (fun (s,t,w, b) -> moveSymbol (checkXCoor s t b |> delta w b) (if b then s else t)) syms
    let symbols' =  (symbolMap, movedSyms)
                    ||> List.fold (fun s movedSym ->
                        s |> Map.map (fun _ v ->
                            match v.Id with
                            | id when id = movedSym.Id -> movedSym
                            | _ -> v
                        ))


    let overlaps = detectOverlaps (Map.toSeq symbols' |> Seq.map snd |> Seq.toList)
    let nonOverlappingSymbols = resolveOverlaps overlaps (Map.toSeq symbols' |> Seq.map snd |> Seq.toList)



    let NewSymbolModel = 
        Optic.set symbols_ 
            (Map.ofSeq (List.zip (Map.keys symbols' |> Seq.toList) nonOverlappingSymbols)) 
            model


    //Getting Wire map to update to new wire positions based on updated Symbol postions
    let wireMap = Optic.get wires_ model
    let movedWires = wireMap |> Map.values |> List.ofSeq
                    |> List.map (fun w ->
                    BusWireUpdateHelpers.autoroute NewSymbolModel.Wire w)
    
    let wires' =  (wireMap, movedWires)
                    ||> List.fold (fun w movedWire ->
                        w |> Map.map (fun _ v ->
                            match v.WId with
                            | id when id = movedWire.WId -> movedWire
                            | _ -> v
                        ))
    

    //Updating the model with the new wire positions
    // let intersectingPair = numOfIntersectedSymPairs NewSymbolModel
    // printfn "Number of intersecting pairs: %d" intersectingPair
    Optic.set wires_ wires' NewSymbolModel



/// <summary>
/// Aligns and scales symbols based on the positions and bounding boxes of connected symbols.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
let sheetAlignScale (model: SheetT.Model) =
    // let symbols = getAllSymbols model
    let syms = getAllSingleConnectedSymbols model (parallelWires model)
    printfn "Running SheetAlign %A" <| List.map (fun(s,t,w,b)->(s.Component.Label, t.Component.Label, w.WId,b)) syms

    let rec runAlignSingleConnectedSyms model symList count = 
        match symList with
        | [] -> model
        | _ -> 
            if List.length symList > 0 && count < 10 then //
                let model' = alignSingleConnectedSyms model symList
                let updatedSymList = getAllSingleConnectedSymbols model' (parallelWires model')
                let symbolsOnly1 = List.map (fun (sym, _, _, _) -> sym) updatedSymList
                let symbolsOnly2 = List.map (fun (_, sym, _, _) -> sym) updatedSymList
                let overlaps1 = detectOverlaps symbolsOnly1
                let overlaps2 = detectOverlaps symbolsOnly2
                let resolvedSymList1 = resolveOverlaps overlaps1 symbolsOnly1
                let resolvedSymList2 = resolveOverlaps overlaps2 symbolsOnly2
                let mappedResolvedSymList1 = List.map2 (fun (_, sym, wire, updated) resolvedSym -> (resolvedSym, sym, wire, updated)) updatedSymList resolvedSymList1
                let mappedResolvedSymList2 = List.map2 (fun (sym, _, wire, updated) resolvedSym -> (sym, resolvedSym, wire, updated)) mappedResolvedSymList1 resolvedSymList2
                runAlignSingleConnectedSyms model' mappedResolvedSymList2 (count + 1)
            else
                model
    // 
    runAlignSingleConnectedSyms model syms 0
