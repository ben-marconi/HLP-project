module SheetBeautifyB2
// open modules likely to be used
open SheetBeautifyHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open BlockHelpers
open Optics
open Helpers
open Symbol

/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists

// Algorithms to minimise wire crossings
// Exhaustive search algorithm

//Function to flip all MUXs on the sheet

let sheetFlipMUX (model: Model) : Model =
    let updatedSymbols =
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (id, symbol) ->
            if symbol.Component.Type = Mux2 then
                (id, Optic.set symbolFlipState_ (not symbol.STransform.Flipped) symbol)
            else
                (id, symbol))
        |> Map.ofList
    { model with
        Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }

let sheetReverseInputMUX (model: Model) : Model =
    let updatedSymbols =
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (id, symbol) ->
            match symbol.Component.Type, symbol.ReversedInputPorts with
            | Mux2, Some(isReversed) ->
                let updatedSymbol = { symbol with ReversedInputPorts = Some(not isReversed) }
                (id, updatedSymbol)
            | _ -> (id, symbol))
        |> Map.ofList
    { model with
        Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbols } } }

let sheetFunctionEvaluate (model: Model) func : Model =
    let preFlipCrossingCount = countRightAngleCrossings model
    let preFlipRightAnglesCount = countVisibleWireRightAngles model

    let transformedModel = func model

    let postFlipCrossingCount = countRightAngleCrossings flippedModel
    let postFlipRightAnglesCount = countVisibleWireRightAngles flippedModel

    if
        postFlipCrossingCount < preFlipCrossingCount
        && postFlipRightAnglesCount
           <= preFlipRightAnglesCount
    then
        transformedModel
    else
        model
