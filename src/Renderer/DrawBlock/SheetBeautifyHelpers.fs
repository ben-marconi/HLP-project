module SheetBeautifyHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Symbol
open Optics


// B1
// Read/write the dimensions of a custom comp symbol
let customComponentDimensions_  =
    Lens.create
        (fun (sym : Symbol) ->
        (Option.defaultValue 1.0 sym.HScale) * sym.Component.W, (Option.defaultValue 1.0 sym.VScale) * sym.Component.H)
        (fun (newH, newW) sym ->
        let updatedComponent = { sym.Component with H = newH; W = newW }
        { sym with Component = updatedComponent })

// B2
// Write the position of a symbol on the sheet
let writeSymbolPos (offset: XYPos) (symbol: Symbol) : Symbol =
    { symbol with
        Pos = symbol.Pos + offset
        Component = { symbol.Component with
                        X = symbol.Component.X + offset.X
                        Y = symbol.Component.Y + offset.Y }
        LabelBoundingBox = { symbol.LabelBoundingBox with
                                TopLeft = symbol.LabelBoundingBox.TopLeft + offset }
    }


// B3
// Read/write the order of ports on a specified side of a symbol
let symbolPortOrder_ (side: Edge) : Lens<Symbol, string list> = 
    Lens.create
        (fun (symbol: Symbol) ->
            match Map.tryFind side symbol.PortMaps.Order with
            | Some ports -> ports
            | None -> [] 
        )
        (fun (newOrder: string list) (symbol: Symbol) ->
            let updatedOrder = Map.add side newOrder symbol.PortMaps.Order
            let updatedPortMaps = { symbol.PortMaps with Order = updatedOrder }
            { symbol with PortMaps = updatedPortMaps }
        )

// B4
// Read/write the reverses state of the inputs of a MUX2
let reverseInputPorts_ = 
    Lens.create
        (fun (symbol: Symbol) ->
        match symbol.ReversedInputPorts with 
        | None -> failwithf "Reversed Input Ports not found"
        | Some value -> value
        )
        (fun (newReversedInputState: bool) (symbol: Symbol) ->
            { symbol with ReversedInputPorts = Some newReversedInputState }
        )


// B5
// Read the position of a port on the sheet. It cannot directly be written
let getPortPosition (symbol: Symbol) (portId: string) : Option<XYPos> =
    let comp = symbol.Component
    let maybeEdge = symbol.PortMaps.Orientation |> Map.tryFind portId
    let portsOnEdge = maybeEdge |> Option.bind (fun edge -> symbol.PortMaps.Order |> Map.tryFind edge)
    match maybeEdge, portsOnEdge with
    | Some edge, Some ports ->
        let index = ports |> List.tryFindIndex (fun id -> id = portId)
        match index with
        | None -> None
        | Some index ->
            let gap, basePos = 
                match edge with
                | Top -> comp.W / float (List.length ports + 1), comp.Y
                | Bottom -> comp.W / float (List.length ports + 1), comp.Y - comp.H
                | Left -> comp.H / float (List.length ports + 1), comp.X
                | Right -> comp.H / float (List.length ports + 1), comp.X + comp.W
            let offset = gap * float (index + 1)
            match edge with
            | Top | Bottom -> Some { X = comp.X + offset; Y = basePos }
            | Left | Right -> Some { X = basePos; Y = comp.Y - offset }
    | _ -> None
    
// B6
// The Bounding box of a symbol outline (position is contained in this)
let getBoundingBox (symbol: Symbol) = 
    let h,w = getRotatedHAndW symbol
    if symbol.Annotation = Some ScaleButton then 
        {TopLeft = symbol.Pos - {X = 9.; Y = 9.}; H = 17. ; W = 17.}
    else 
        {TopLeft = symbol.Pos; H = float(h) ; W = float(w)}

// B7
// Read/Write The rotation state of a symbol
let symbolRotation_ = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (rotation: Rotation) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Rotation = rotation}
        { symbol with STransform = newSTransform })


// B8
// Read/Write The flip state of a symbol
let symbolFlip_ = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Flipped)
        (fun (newFlip: bool) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Flipped = newFlip}
        { symbol with STransform = newSTransform })