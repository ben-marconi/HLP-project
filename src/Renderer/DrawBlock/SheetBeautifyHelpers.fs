module SheetBeautifyHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics


// Read/write the dimensions of a custom comp symbol
let B1  =
    Lens.create
        (fun (sym : Symbol) ->
        (Option.defaultValue 1.0 sym.HScale) * sym.Component.W, (Option.defaultValue 1.0 sym.VScale) * sym.Component.H)
        (fun (newH, newW) sym ->
        let updatedComponent = { sym.Component with H = newH; W = newW }
        { sym with Component = updatedComponent })

// Write the position of a symbol on the sheet
let B2 (offset: XYPos) (symbol: Symbol) : Symbol =
    { symbol with
        Pos = symbol.Pos + offset
        Component = { symbol.Component with
                        X = symbol.Component.X + offset.X
                        Y = symbol.Component.Y + offset.Y }
        LabelBoundingBox = { symbol.LabelBoundingBox with
                                TopLeft = symbol.LabelBoundingBox.TopLeft + offset }
    }


// Read/write the order of ports on a specified side of a symbol
let B3 (side: Edge) : Lens<Symbol, string list> = 
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

// Read/write the reverses state of the inputs of a MUX2
// let B4



// Read the position of a port on the sheet. It cannot directly be written
let B5 (symbol: Symbol) (portId: string) : Option<XYPos> =
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
                | Top -> comp.W / float (List.length ports + 1), comp.Y + comp.H / 2.0
                | Bottom -> comp.W / float (List.length ports + 1), comp.Y - comp.H / 2.0
                | Left -> comp.H / float (List.length ports + 1), comp.X - comp.W / 2.0
                | Right -> comp.H / float (List.length ports + 1), comp.X + comp.W / 2.0
            let offset = gap * float (index + 1)
            match edge with
            | Top | Bottom -> Some { X = (comp.X - comp.W / 2.0) + offset; Y = basePos }
            | Left | Right -> Some { X = basePos; Y = (comp.Y + comp.H / 2.0) - offset }
    | _ -> None
    
