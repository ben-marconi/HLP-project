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


// B1: Read/write the dimensions of a custom comp symbol
let customSymDim  =
    Lens.create
        (fun (sym : Symbol) ->
        ((Option.defaultValue 1.0 sym.HScale) * sym.Component.W, 
         (Option.defaultValue 1.0 sym.VScale) * sym.Component.H))

        (fun (newH: float, newW: float) (sym: Symbol) ->
        {sym with HScale = Some (newH/sym.Component.H); VScale = Some (newW/sym.Component.W)})

// B2: Write the position of a symbol on the sheet
let symbolPos (updatedPos: XYPos) (symbol: Symbol) : Symbol =
    { symbol with
        Pos = updatedPos
        Component = { symbol.Component with
                        X = updatedPos.X
                        Y = updatedPos.Y }
        LabelBoundingBox = { symbol.LabelBoundingBox with
                                TopLeft = updatedPos }
    }

// B3
let symPortOrder (side: Edge) : Lens<Symbol, string list> = 
    Lens.create
        (fun symbol -> 
                    match Map.tryFind side symbol.PortMaps.Order with
                    | Some ports -> ports
                    | None -> [] )
        (fun newOrder symbol ->
            { symbol with PortMaps = { symbol.PortMaps with Order = Map.add side newOrder symbol.PortMaps.Order } })

// B4
let revMuxInput = 
    Lens.create
        (fun (symbol: Symbol) ->
        symbol.ReversedInputPorts |> Option.defaultValue false)

        (fun (newReversedInputState: bool) (symbol: Symbol) ->
            { symbol with ReversedInputPorts = Some newReversedInputState }
        )

// B5
let getPortPos (sym: Symbol) (port: Port) : XYPos =
    let TopLeftPos = sym.Pos
    let offset = getPortPos sym port
    { X = TopLeftPos.X + offset.X; Y = TopLeftPos.Y + offset.Y }

// B6
let getBoundingBox ({ Pos = pos; Component = { W = w; H = h }; HScale = hScale; VScale = vScale } : Symbol) =
    {
        TopLeft = pos
        W = Option.defaultValue 1.0 hScale * w
        H = Option.defaultValue 1.0 vScale * h
    }

// B7
let symRotation = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (newRotation: Rotation) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Rotation = newRotation}
        { symbol with STransform = newSTransform })

// B8
let symFlip = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Flipped)
        (fun (newFlip: bool) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Flipped = newFlip}
        { symbol with STransform = newSTransform })


// T1
// The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols.
let countIntersectingSymbolPairs (sheet: SheetT.Model) : int =
    let boxes =
        Map.values sheet.BoundingBoxes
        |> Seq.toList
        |> List.mapi (fun n box -> n, box)


    List.allPairs boxes boxes
    |> List.filter (fun ((n1, box1), (n2, box2)) -> n1 <> n2 && BlockHelpers.overlap2DBox box1 box2)
    |> List.length


// T3
// The number of pairs of symbols that intersect each other. See Tick3 for a related function. Count over all pairs of symbols.
let getSegmentOrientation (index: int) (initialOrientation: Orientation) : Orientation =
    match index % 2, initialOrientation with
    | 0, Horizontal | 1, Vertical -> Horizontal
    | _ -> Vertical

let intersectsRightAngle (hSegment: ASegment) (vSegment: ASegment) : bool =
    let hStartX, hEndX = min hSegment.Start.X hSegment.End.X, max hSegment.Start.X hSegment.End.X
    let vStartY, vEndY = min vSegment.Start.Y vSegment.End.Y, max vSegment.Start.Y vSegment.End.Y

    let xIntersects = hSegment.Start.Y >= vStartY && hSegment.Start.Y <= vEndY
    let yIntersects = vSegment.Start.X >= hStartX && vSegment.Start.X <= hEndX

    xIntersects && yIntersects

let determineOrientation (segment: ASegment) : Orientation =
    if segment.Start.Y = segment.End.Y then Horizontal
    else if segment.Start.X = segment.End.X then Vertical
    else 
        // Log a warning message and default to Horizontal
        printfn "Warning: Segment orientation is ambiguous, defaulting to Horizontal."
        Horizontal

let countRightAngleIntersections (model: SheetT.Model) : int =
    let segmentPairs =
        model.Wire.Wires
        |> Map.toSeq
        |> Seq.collect (fun (wireId1, wire1) ->
            model.Wire.Wires
            |> Map.toSeq
            |> Seq.collect (fun (wireId2, wire2) ->
                if wireId1 < wireId2 then
                    let segments1 = BlockHelpers.getNonZeroAbsSegments wire1
                    let segments2 = BlockHelpers.getNonZeroAbsSegments wire2
                    List.allPairs segments1 segments2
                else
                    []
            )
        )

    segmentPairs
    |> Seq.filter (fun (seg1, seg2) ->
        // Assume getSegmentOrientation and intersectsRightAngle are defined elsewhere
        // Determine the orientations based on segment properties or additional data
        let orientation1 = determineOrientation seg1 // Replace with actual logic to determine orientation
        let orientation2 = determineOrientation seg2 // Replace with actual logic to determine orientation
        match orientation1, orientation2 with
        | Horizontal, Vertical | Vertical, Horizontal -> intersectsRightAngle seg1 seg2
        | _ -> false
    )
    |> Seq.length