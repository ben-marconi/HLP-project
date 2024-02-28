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
open Optics.Optic

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
//B4
let reverseInputPort_ = Lens.create (fun(symbol: Symbol) ->
                                            match symbol.ReversedInputPorts with
                                            | Some reversed -> reversed
                                            | None -> failwithf "Symbol %s does not have reversed input ports" symbol.Component.Label)
                                    (fun(reversed: bool) (symbol: Symbol) ->
                                            {symbol with ReversedInputPorts = Some reversed}
                                            )
//B6
let boundingBox (model:Model, symbol:Symbol) = 
    Map.tryFind symbol.Id model.BoundingBoxes

//B7

let rotationStateSymbol_ = Lens.create (fun(symbol: Symbol) -> symbol.STransform.Rotation)
                                    (fun(newRotation: Rotation) (symbol: Symbol) ->
                                        {symbol with STransform = ({symbol.STransform with Rotation = newRotation})})

//B8
let flipStateSymbol_ = Lens.create (fun(symbol: Symbol) -> symbol.STransform.Flipped)
                                    (fun(newFlipped: bool) (symbol: Symbol) ->
                                        {symbol with STransform = ({symbol.STransform with Flipped = newFlipped})})