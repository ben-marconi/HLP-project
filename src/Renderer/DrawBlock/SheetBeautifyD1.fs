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
    |> List.filter (fun wire -> List.length (visibleSegments wire.WId model) = 3)

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

    getSingleConnectedSymbols sourceTargetSymbols

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
    |> List.map (fun (symS, symT, _) -> getSymBoundingBox symS)

/// <summary>
/// Aligns all singly connected symbols by their target port.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols</param>
/// <returns>Updated model with the moved symbols</returns>
let alignSingleConnectedSyms (model: SheetT.Model) =
    let syms = getAllSingleConnectedSymbols model (parallelWires model)
                //Prefer moving symbols with only one port to have minimal impact on the rest of the sheet
                //Need to modify the delta function to account for different directions when moving source/target
                // |> List.map (fun (s,t,w) ->
                //                 printfn $"{s.PortMaps.Order.Count}, {t.PortMaps.Order.Count}"
                //                 match (s.PortMaps.Order.Count, t.PortMaps.Order.Count) with
                //                 | (1,1) -> (s,w)
                //                 | (_, 1) -> (t,w)
                //                 | (_, _) -> (s,w)
                //             )
    let delta (wire:Wire) =
        match wire.InitialOrientation with
        | Horizontal -> {X=0; Y=wire.EndPos.Y - wire.StartPos.Y}
        | Vertical -> {X=wire.EndPos.X - wire.StartPos.X; Y=0}

    let symbolMap = Optic.get symbols_ model
    let movedSyms = List.map (fun (s,t,w) -> moveSymbol (delta w) s) syms
    let symbols' =  (symbolMap, movedSyms)
                    ||> List.fold (fun s movedSym ->
                        s |> Map.map (fun _ v ->
                            match v.Id with
                            | id when id = movedSym.Id -> movedSym
                            | _ -> v
                        ))
    Optic.set symbols_ symbols' model


/// <summary>
/// Aligns and scales symbols based on the positions and bounding boxes of connected symbols.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
let sheetAlignScale (model: SheetT.Model) =
    let symbols = getAllSymbols model
    let boundingBoxes = getSymbolsBoundingBox model |> List.choose (fun bb -> Some(bb))

    let rec groupInPairs list =
        match list with
        | x :: y :: rest -> (x, y) :: groupInPairs rest
        | [x] -> [(x, x)]
        | _ -> []

    let pairsBoundingBox = groupInPairs boundingBoxes
    symbols
    |> List.map (fun (symS, symT, _) -> updateSymPosInSheet symS.Id symT.Pos)
