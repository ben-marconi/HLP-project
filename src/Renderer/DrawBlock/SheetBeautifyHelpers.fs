module SheetBeautifyHelpers
open Fable.Core
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Fable.React
open SheetUpdateHelpers
open Optics
open Operators
open Symbol
open Elmish
open EEExtensions

open DrawModelType.SymbolT


// Read/write the dimensions of a custom comp symbol
let B1  =
    Lens.create
        (fun (sym : Symbol) ->
        ((Option.defaultValue 1.0 sym.HScale) * sym.Component.W, 
         (Option.defaultValue 1.0 sym.VScale) * sym.Component.H))

        (fun (newH: float, newW: float) (sym: Symbol) ->
        {sym with HScale = Some (newH/sym.Component.H); VScale = Some (newW/sym.Component.W)})

// Write the position of a symbol on the sheet
let B2 (offset: XYPos) (symbol: Symbol) : Symbol =
    let updatePos (pos: XYPos) = pos + offset
    { symbol with
        Pos = updatePos symbol.Pos
        Component = { symbol.Component with
                        X = symbol.Component.X + offset.X
                        Y = symbol.Component.Y + offset.Y }
        LabelBoundingBox = { symbol.LabelBoundingBox with
                                TopLeft = symbol.LabelBoundingBox.TopLeft + offset }
    }

let B3 (side: Edge) : Lens<Symbol, string list> = 
    Lens.create
        (fun symbol -> 
                    match Map.tryFind side symbol.PortMaps.Order with
                    | Some ports -> ports
                    | None -> [] )
        (fun newOrder symbol ->
            { symbol with PortMaps = { symbol.PortMaps with Order = Map.add side newOrder symbol.PortMaps.Order } })


let B4 = 
    Lens.create
        (fun (symbol: Symbol) ->
        symbol.ReversedInputPorts |> Option.defaultValue false)

        (fun (newReversedInputState: bool) (symbol: Symbol) ->
            { symbol with ReversedInputPorts = Some newReversedInputState }
        )

let B5 (sym: Symbol) (port: Port) : XYPos =
    let TopLeftPos = sym.Pos
    let offset = getPortPos sym port
    { X = TopLeftPos.X + offset.X; Y = TopLeftPos.Y - offset.Y }

let B6 ({ Pos = pos; Component = { W = w; H = h }; HScale = hScale; VScale = vScale } : Symbol) =
    {
        TopLeft = pos
        W = Option.defaultValue 1.0 hScale * w
        H = Option.defaultValue 1.0 vScale * h
    }


let B7 = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (newrotation: Rotation) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Rotation = newrotation}
        { symbol with STransform = newSTransform })

let B8 = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Flipped)
        (fun (newFlip: bool) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Flipped = newFlip}
        { symbol with STransform = newSTransform })