module SheetBeautify
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

/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists



let parallelWires (model: SheetT.Model) =
        Map.values model.Wire.Wires
        |> List.ofSeq
        |> List.filter (fun x -> List.length (visibleSegments x.WId model) = 3)

let getAllSingleConnectedSymbols (model: SheetT.Model) (wires: Wire list) =              

    let SourceTargetSymbols = 
        List.map (fun wire -> ((getSourceSymbol model.Wire wire),getTargetSymbol model.Wire wire, wire)) wires
    // let sourceSymbols = 
    //         List.map (fun wire -> (getSourceSymbol model.Wire wire, wire)) wires
    
    let getSingleConnectedSymbols (symbolList: (Symbol * Symbol * Wire) list) = 
        symbolList
        |> List.groupBy (fun sym -> sym)
        |> List.filter (fun (_,sym_list) -> List.length sym_list = 1)
        |> List.map (fun (sym,_) -> sym)
    
    let singleConnectedSymbols =
        getSingleConnectedSymbols SourceTargetSymbols
    
    singleConnectedSymbols

let getAllSymbols (model: SheetT.Model) =
    getAllSingleConnectedSymbols model (parallelWires model)
    // |> List.length
    
let getSymbolsBoundingBox (model: SheetT.Model) = 
    let symbols = getAllSymbols model
    symbols
    |> List.map (fun (symS,symT,_) -> getSymbolBoundingBox model symS)

let sheetAlignScale (model: SheetT.Model) =
    let symbols = getAllSymbols model
    let boundingBoxes = getSymbolsBoundingBox model
                                                |> List.choose (fun bb -> bb)

    let rec groupInPairs list =
        match list with
        | x :: y :: rest -> (x, y) :: groupInPairs rest
        | [x] -> [(x,x)]
        | _ -> []
    let pairs_boundingBox = groupInPairs boundingBoxes
    symbols
    |> List.map (fun (symS,symT,_) -> 
    changeSymbolPosition symS (symT.Pos)
    )


    