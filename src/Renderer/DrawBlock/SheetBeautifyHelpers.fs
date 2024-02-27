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

open Symbol //added -check

//B1R, B1W - Read and Write the dimensions of a custom component symbol
// Partially Complete
//READ EDSTEM -> SAYS VALUE WRITTEN WILL BE HEIGHT AND WIDTH HERE - but its a custom component symbol and symbol includes scaling?
let customComponentDimensions_ = 
    Lens.create
        (fun (sym: Symbol) -> 
            let hS, vS = (Option.defaultValue 1.0 sym.HScale),(Option.defaultValue 1.0 sym.VScale)
            (hS*sym.Component.H, vS*sym.Component.W))
        (fun (newHeight: float, newWidth: float) (sym: Symbol) -> 
            {sym with HScale = Some (newHeight/sym.Component.H); VScale = Some (newWidth/sym.Component.W)})
//Should it be hscale input or height and converted as i did 

//B2W - Write the position of a symbol on the sheet
let writePosition (sym: Symbol) (newPos: XYPos) = {sym with Pos = newPos}

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

let portPos (model: Model) (portId: string) : XYPos = 
    getPortLocation None model portId              //what is the difference between sheetT.model and symbolT.model

//wireSymEdge in BlockHelpers.fs?
//getPort in BlockHelpers.fs?
// GETpORTpOS  - TOP LEFT OF SO symbol.pos + getportpose symbol port


//B6R - The Bounding box of a symbol outline (position is contained in this)
let symbolOutlineBoundingBox (sym: Symbol) : BoundingBox = getSymbolBoundingBox sym
// Surely this is not correct - is it something to do with it having Symbol outline


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