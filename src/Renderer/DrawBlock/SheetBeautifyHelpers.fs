module SheetBeautifyHelpers
open BlockHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics
open Operators
open Symbol
open Elmish
open EEExtensions

open DrawModelType.SymbolT

/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let getVisibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =
    let wire = model.Wire.Wires.[wId]

    let getSegmentVector index seg =
        match index % 2, wire.InitialOrientation with
        | 0, BusWireT.Vertical | 1, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
        | _ -> { X = seg.Length; Y = 0. }

    let rec coalesceSegments vectors acc index =
        match vectors with
        | prev :: zeroLength :: next :: tail when zeroLength = XYPos.zero ->
            let combined = prev + next
            coalesceSegments (combined :: tail) (acc @ [prev]) (index + 2)
        | head :: tail -> coalesceSegments tail (acc @ [head]) (index + 1)
        | [] -> acc

    wire.Segments
    |> List.indexed
    |> List.map (fun (index, seg) -> getSegmentVector index seg)
    |> fun vectors -> coalesceSegments vectors [] 0



// B1
/// <summary> Read/write the dimensions of a custom component symbol. </summary>
/// <param name="sym">The symbol to access or modify.</param>
/// <returns>
/// A lens for manipulating the component dimensions of the symbol.
/// </returns>
let customSymDim  =
    Lens.create
        (fun (sym : Symbol) ->
        ((Option.defaultValue 1.0 sym.HScale) * sym.Component.W, 
         (Option.defaultValue 1.0 sym.VScale) * sym.Component.H))

        (fun (newH: float, newW: float) (sym: Symbol) ->
        {sym with HScale = Some (newH/sym.Component.H); VScale = Some (newW/sym.Component.W)})

// B2
/// <summary> Updates the position of a symbol on the sheet. </summary>
/// <param name="updatedPos">The new position for the symbol.</param>
/// <param name="symbol">The symbol to update.</param>
/// <returns>The symbol with the updated position.</returns>
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
/// <summary> Provides read/write access to the order of ports on a specified side of a symbol. </summary>
/// <param name="side">The side of the symbol to access the port order for.</param>
/// <returns>A lens for manipulating the port order on a symbol's specific side.</returns>
let symPortOrder (side: Edge) : Lens<Symbol, string list> = 
    Lens.create
        (fun symbol -> 
                    match Map.tryFind side symbol.PortMaps.Order with
                    | Some ports -> ports
                    | None -> [] )
        (fun newOrder symbol ->
            { symbol with PortMaps = { symbol.PortMaps with Order = Map.add side newOrder symbol.PortMaps.Order } })

// B4
/// <summary> Read/write the reversed state of the input ports of a MUX2 symbol. </summary>
/// <remarks> Throws an exception if the symbol does not have reversed input ports defined. </remarks>
let revMuxInput = 
    Lens.create
        (fun (symbol: Symbol) ->
        symbol.ReversedInputPorts |> Option.defaultValue false)

        (fun (newReversedInputState: bool) (symbol: Symbol) ->
            { symbol with ReversedInputPorts = Some newReversedInputState }
        )

// B5
/// <summary> Gets the position of a specified port on a symbol within the sheet.</summary>
/// <param name="sym">The symbol containing the port.</param>
/// <param name="port">The id of the port whose position is being queried.</param>
/// <returns>The position of the port.</returns>
let getPortPos (sym: Symbol) (port: Port) : XYPos =
    let TopLeftPos = sym.Pos
    let offset = getPortPos sym port
    { X = TopLeftPos.X + offset.X; Y = TopLeftPos.Y + offset.Y }

// B6
/// <summary>Returns the bounding box of a symbol's outline</summary>
/// <param name="Pos">The symbol's initial position.</param>
/// <param name="Component">Contains the height and weight of the component.</param>
/// <param name="HScale">The scale of height.</param>
/// <param name="VScale">The scale of width.</param>
/// <returns>The bounding box of the symbol. </returns>
let getBoundingBox ({ Pos = pos; Component = { W = w; H = h }; HScale = hScale; VScale = vScale } : Symbol) =
    {
        TopLeft = pos
        W = Option.defaultValue 1.0 hScale * w
        H = Option.defaultValue 1.0 vScale * h
    }

// B7
/// <summary>Reads or writes the rotation state of a symbol.</summary>
/// <param name="symbol">The symbol to modify or read the rotation from.</param>
/// <param name="newRotation">The new rotation state to apply to the symbol.</param>
/// <returns>The updated symbol with the new rotation state.</returns>
let symRotation = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Rotation)
        (fun (newRotation: Rotation) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Rotation = newRotation}
        { symbol with STransform = newSTransform })

// B8
/// <summary>Reads or writes the flip state of a symbol.</summary>
/// <param name="symbol">The symbol to modify or read the flip state from.</param>
/// <param name="newFlip">The new flip state to apply to the symbol.</param>
/// <returns>The updated symbol with the new flip state.</returns>
let symFlip = 
    Lens.create
        (fun (symbol: Symbol) -> symbol.STransform.Flipped)
        (fun (newFlip: bool) (symbol: Symbol) -> 
        let newSTransform = {symbol.STransform with Flipped = newFlip}
        { symbol with STransform = newSTransform })


// T1
/// <summary>Counts the number of pairs of symbols that intersect each other on a sheet.</summary>
/// <param name="sheet">The sheet model containing the symbols.</param>
/// <returns>The total number of intersecting symbol pairs.</returns>
let countIntersectingSymbolPairs (sheet: SheetT.Model) : int =
    let boxes =
        Map.values sheet.BoundingBoxes
        |> Seq.toList
        |> List.mapi (fun n box -> n, box)


    List.allPairs boxes boxes
    |> List.filter (fun ((n1, box1), (n2, box2)) -> n1 <> n2 && BlockHelpers.overlap2DBox box1 box2)
    |> List.length


// T2
/// <summary>Counts the number of distinct wire visible segments that intersect with one or more symbols on a sheet.</summary>
/// <param name="model">The sheet model containing the wires and symbols.</param>
/// <returns>The sum of visible wire segment intersections with symbols.</returns>
let countSegmentSymbolIntersections (model: SheetT.Model): int =
    let busWireModel = model.Wire  // Access the nested BusWireT.Model
    let convertToSegments wireId wire (posList: XYPos list) =
        posList |> List.mapi (fun idx pos ->
            { Index = idx; 
              Length = if pos.X <> 0.0 then pos.X else pos.Y; 
              WireId = wireId; 
              IntersectOrJumpList = [0.]; // Placeholder for irrelevant attributes
              Draggable = true; 
              Mode = Auto })

    let getInitialOrientation (posList: XYPos list) =
        if (List.head posList).X = 0.0 then Vertical else Horizontal

    let updatedWires = 
        model.Wire.Wires
        |> Map.map (fun wireId wire ->
            let posList = getVisibleSegments wireId model
            let segments = convertToSegments wireId wire posList
            let orientation = getInitialOrientation posList
            { wire with Segments = segments; InitialOrientation = orientation })

    let updatedBusWireModel = { busWireModel with Wires = updatedWires }

    updatedWires
    |> Map.fold (fun acc _ wire -> acc + (BusWireRoute.findWireSymbolIntersections updatedBusWireModel wire).Length) 0


// T3
/// <summary>Counts the number of distinct pairs of segments that cross each other at right angles on a sheet. Multiple functions used. The main function is countRightAngleIntersections.</summary>
/// <param name="model">The sheet model for which to count the right angle segment intersections.</param>
/// <returns>The total number of right angle intersections between segments.The length of a list</returns>
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

// T4 still needs working.





// T5
/// <summary>Counts the number of visible wire right-angles over the whole sheet.</summary>
/// <param name="model">The sheet model to count for visible right-angles formed by wires.</param>
/// <returns>The total number of visible wire right-angle.</returns>
let countWireRightAngles (model: SheetT.Model) : int =
    let countRightAngles wireId = 
        let visibleSegments = getVisibleSegments wireId model
        max (List.length visibleSegments - 1) 0  // Ensure we don't count negative if there's one segment or none

    model.Wire.Wires
    |> Map.keys
    |> Seq.sumBy countRightAngles
