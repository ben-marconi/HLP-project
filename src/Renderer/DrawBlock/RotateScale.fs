(* bam2: worked on functions from flipSymbolInBlock to rotateBlock (lines 441 to 587)

Summary of Most Important Changes:
    1.  In flipSymbolInBlock, I added or reformatted pipelines in most local variable and function definitions (Transform 3).  
        Most importantly, I replaced the final block of code which was a very long pipeline: I broke it up (Transform 3) into 
        a defined identifier that documents an intermediate value and a separate function with a clear identifier (Transform 1).
        These changes worked towards the DUI and LFM Principles.
        
        Changes in Github:
        https://github.com/ben-marconi/HLP-project/commit/98b8acb2667ed8dc740089c129f33daa8f0ed332#diff-9e7a112f68a531037835f68fd5ba2311c4d7248b2e8fd03a1a51bba8f8f99d21R441-R466

    2.  In rotateBlock, I completely changed the the code of the return value of the function. I used a separate identifier to define 
        the new model symbols. In this definition, I used a series of pipeline operators which were not originally used (Transform 3).
        This had the effect of Reducing Syntactic Noise (RSN) and worked towards improving the DUI and LFM principles. 
        
        Changes in Github:
        https://github.com/ben-marconi/HLP-project/commit/98b8acb2667ed8dc740089c129f33daa8f0ed332#diff-9e7a112f68a531037835f68fd5ba2311c4d7248b2e8fd03a1a51bba8f8f99d21R535-R544
    
    3.  In scaleSymbolInBlock, my most important contribution was changing the way the new center of the symbol (newCenter) was
        calculated by abstracting the scaling operation (Transform 1) in line 493. This helped reduce repetition and improve clarity,
        working towards the DRY, RSN and LYM principles. 
        
        Changes in Github
        https://github.com/ben-marconi/HLP-project/commit/98b8acb2667ed8dc740089c129f33daa8f0ed332#diff-9e7a112f68a531037835f68fd5ba2311c4d7248b2e8fd03a1a51bba8f8f99d21R485-R492
    
    4.  I added XML documentation to the rotateSymbolByDegree function (DUI), which was previously missing. I added explanations about what 
        the parameters are and what the function returns. I also renamed an identifier for further clarity (DUI).

        Changes in Github:
        https://github.com/ben-marconi/HLP-project/commit/98b8acb2667ed8dc740089c129f33daa8f0ed332#diff-9e7a112f68a531037835f68fd5ba2311c4d7248b2e8fd03a1a51bba8f8f99d21R501-R505

Transforms Used:
    1.  Functional Abstraction
        a.  flipSymbolInBlock: Introduced applyFlip function to abstract flipping logic, making the main function more focused and simpler.
        b.  rotateBlock: Abstracted the process of updating symbols after rotation into clearer, distinct steps, simplifying the overall logic.
        c.  scaleSymbolInBlock: Abstracted the scaling operation by creating a seperate match condition
    
    3.  Pipelines
        a.  flipSymbolInBlock: I broke down a very long pipeline into an identifier and a separate function. I also used pipelines in the definition 
            of updatedTopLeft and flippedPortOrientation. 
        b.  rotateBlock: In this function I used a separate identifier to define the new model symbols. In this definition, I used a series of 
            pipeline operators which were not originally used and made the code much clearer in doing so.
*)


module RotateScale
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SymbolUpdate
open Symbol
open Optics
open Operators
open BlockHelpers
open SymbolResizeHelpers

(* 
    This module contains the code that rotates and scales blocks of components.
    It was collected from HLP work in 2023 and has some technical debt and also unused functions.
    It requires better documentation of teh pasrts now used.
*)

/// Record containing all the information required to calculate the position of a port on the sheet.
type PortInfo =
    { port: Port
      sym: Symbol
      side: Edge
      ports: string list
      gap: float
      topBottomGap: float
      portDimension: float
      h: float
      w: float
      portGap: float }

/// Type used to record a wire between two symbols.
type WireSymbols =
    { SymA: Symbol
      SymB: Symbol
      Wire: Wire }

/// TODO: this is mostly copy pasted code from Symbol.getPortPos, perhaps abstract out the existing code there to use makePortInfo.
/// Could not simply use getPortPos because more data (side, topBottomGap, etc.) is needed to caclulate the new dimensions of the resized symbol.
let makePortInfo (sym: Symbol) (port: Port) =
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let gap = getPortPosEdgeGap sym.Component.Type
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let portDimension = float ports.Length - 1.0
    let h, w = getRotatedHAndW sym

    let portGap =
        match side with
        | Left
        | Right -> float h / (portDimension + 2.0 * gap)
        | Bottom
        | Top -> float w / (portDimension + 2.0 * topBottomGap)

    { port = port
      sym = sym
      side = side
      ports = ports
      gap = gap
      topBottomGap = topBottomGap
      portDimension = portDimension
      h = h
      w = w
      portGap = portGap }

let getPortAB wModel wireSyms =
    let ports = portsOfWires wModel [ wireSyms.Wire ]
    let portA = filterPortBySym ports wireSyms.SymA |> List.head
    let portB = filterPortBySym ports wireSyms.SymB |> List.head
    portA, portB

/// Try to get two ports that are on opposite edges.
let getOppEdgePortInfo
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : (PortInfo * PortInfo) option =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    let tryGetOppEdgePorts wireSyms =
        let portA, portB = getPortAB wModel wireSyms
        let edgeA = getSymbolPortOrientation wireSyms.SymA portA
        let edgeB = getSymbolPortOrientation wireSyms.SymB portB

        match edgeA = edgeB.Opposite with
        | true -> Some(makePortInfo wireSyms.SymA portA, makePortInfo wireSyms.SymB portB)
        | _ -> None

    wires
    |> List.tryPick (fun w ->
        tryGetOppEdgePorts
            { SymA = symbolToSize
              SymB = otherSymbol
              Wire = w })

let alignPortsOffset (movePInfo: PortInfo) (otherPInfo: PortInfo) =
    let getPortRealPos pInfo =
        getPortPos pInfo.sym pInfo.port + pInfo.sym.Pos

    let movePortPos = getPortRealPos movePInfo
    let otherPortPos = getPortRealPos otherPInfo
    let posDiff = otherPortPos - movePortPos

    match movePInfo.side with
    | Top
    | Bottom -> { X = posDiff.X; Y = 0.0 }
    | Left
    | Right -> { X = 0.0; Y = posDiff.Y }

let alignSymbols
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =

    // Only attempt to align symbols if they are connected by ports on parallel edges.
    match getOppEdgePortInfo (wModel:BusWireT.Model) symbolToSize otherSymbol with
    | None -> wModel
    | Some(movePortInfo, otherPortInfo) ->
        let offset = alignPortsOffset movePortInfo otherPortInfo
        let symbol' = moveSymbol offset symbolToSize
        let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
        BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
let reSizeSymbol (wModel: BusWireT.Model) (symbolToSize: Symbol) (otherSymbol: Symbol) : (Symbol) =
    let wires = wiresBtwnSyms wModel symbolToSize otherSymbol

    // Try to get two ports that are on opposite edges, if none found just use any two ports.
    let resizePortInfo, otherPortInfo =
        match getOppEdgePortInfo wModel symbolToSize otherSymbol with
        | None ->
            let pA, pB = getPortAB wModel { SymA = symbolToSize; SymB = otherSymbol; Wire = wires[0] }
            makePortInfo symbolToSize pA, makePortInfo symbolToSize pB
        | Some(pIA, pIB) -> (pIA, pIB)

    let h, w =
        match resizePortInfo.side with
        | Left | Right ->
            otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.gap), resizePortInfo.w
        | Top | Bottom ->
            resizePortInfo.h, otherPortInfo.portGap * (resizePortInfo.portDimension + 2.0 * resizePortInfo.topBottomGap)

    match symbolToSize.Component.Type with
    | Custom _ ->
        let scaledSymbol = setCustomCompHW h w symbolToSize
        let scaledInfo = makePortInfo scaledSymbol resizePortInfo.port
        let offset = alignPortsOffset scaledInfo otherPortInfo
        moveSymbol offset scaledSymbol
    | _ ->
        symbolToSize

/// For UI to call ResizeSymbol.
let reSizeSymbolTopLevel
    (wModel: BusWireT.Model)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    let scaledSymbol = reSizeSymbol wModel symbolToSize otherSymbol

    let model' = Optic.set (symbolOf_ symbolToSize.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id

/// For each edge of the symbol, store a count of how many connections it has to other symbols.
type SymConnDataT =
    { ConnMap: Map<ComponentId * Edge, int> }

/// If a wire between a target symbol and another symbol connects opposite edges, return the edge that the wire is connected to on the target symbol 
let tryWireSymOppEdge (wModel: Model) (wire: Wire) (sym: Symbol) (otherSym: Symbol) =
    let symEdge = wireSymEdge wModel wire sym
    let otherSymEdge = wireSymEdge wModel wire otherSym

    match symEdge = otherSymEdge.Opposite with
    | true -> Some symEdge
    | _ -> None

let updateOrInsert (symConnData: SymConnDataT) (edge: Edge) (cid: ComponentId) =
    let m = symConnData.ConnMap
    let count = Map.tryFind (cid, edge) m |> Option.defaultValue 0 |> (+) 1
    { ConnMap = Map.add (cid, edge) count m }

// TODO: this is copied from Sheet.notIntersectingComponents. It requires SheetT.Model, which is not accessible from here. Maybe refactor it.
let noSymbolOverlap (boxesIntersect: BoundingBox -> BoundingBox -> bool) boundingBoxes sym =
    let symBB = getSymbolBoundingBox sym

    boundingBoxes
    |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox symBB && sym.Id <> sId)
    |> Map.isEmpty

/// Finds the optimal size and position for the selected symbol w.r.t. to its surrounding symbols.
let optimiseSymbol
    (wModel: BusWireT.Model)
    (symbol: Symbol)
    (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>)
    : BusWireT.Model =

    // If a wire connects the target symbol to another symbol, note which edge it is connected to
    let updateData (symConnData: SymConnDataT) _ (wire: Wire) =
        let symS, symT = getSourceSymbol wModel wire, getTargetSymbol wModel wire

        let otherSymbol =
            match symS, symT with
            | _ when (symS.Id <> symbol.Id) && (symT.Id = symbol.Id) -> Some symS
            | _ when (symS = symbol) && (symT <> symbol) -> Some symT
            | _ -> None

        match otherSymbol with
        | Some otherSym ->
            let edge = tryWireSymOppEdge wModel wire symbol otherSym

            match edge with
            | Some e -> updateOrInsert symConnData e otherSym.Id
            | None -> symConnData // should not happen
        | None -> symConnData 

    // Look through all wires to build up SymConnDataT.
    let symConnData = ({ ConnMap = Map.empty }, wModel.Wires) ||> Map.fold updateData

    let tryResize (symCount: ((ComponentId * Edge) * int) array) sym =

        let alignSym (sym: Symbol) (otherSym: Symbol) =
            let resizedSym = reSizeSymbol wModel sym otherSym
            let noOverlap = noSymbolOverlap DrawHelpers.boxesIntersect boundingBoxes resizedSym

            match noOverlap with
            | true -> true, resizedSym
            | _ -> false, sym

        let folder (hAligned, vAligned, sym) ((cid, edge), _) =
            let otherSym = Optic.get (symbolOf_ cid) wModel       

            match hAligned, vAligned with
            | false, _ when edge = Top || edge = Bottom ->
                let hAligned', resizedSym = alignSym sym otherSym
                (hAligned', vAligned, resizedSym)
            | _, false when edge = Left || edge = Right ->
                let vAligned', resizedSym = alignSym sym otherSym
                (hAligned, vAligned', resizedSym)
            | _ -> (hAligned, vAligned, sym)

        let (_, _, sym') = ((false, false, sym), symCount) ||> Array.fold folder
        sym'

    let scaledSymbol =
        let symCount =
            Map.toArray symConnData.ConnMap
            |> Array.filter (fun (_, count) -> count > 1)
            |> Array.sortByDescending snd

        tryResize symCount symbol

    let model' = Optic.set (symbolOf_ symbol.Id) scaledSymbol wModel
    BusWireSeparate.routeAndSeparateSymbolWires model' symbol.Id

/// <summary>HLP 23: AUTHOR Ismagilov - Get the bounding box of multiple selected symbols</summary>
/// <param name="symbols"> Selected symbols list</param>
/// <returns>Bounding Box</returns>
let getBlock 
        (symbols:Symbol List) :BoundingBox = 

    let maxXsym = (List.maxBy (fun (x:Symbol) -> x.Pos.X+(snd (getRotatedHAndW x))) symbols)
    let maxX = maxXsym.Pos.X + (snd (getRotatedHAndW maxXsym))

    let minX = (List.minBy (fun (x:Symbol) -> x.Pos.X) symbols).Pos.X

    let maxYsym = List.maxBy (fun (x:Symbol) -> x.Pos.Y+(fst (getRotatedHAndW x))) symbols
    let maxY = maxYsym.Pos.Y + (fst (getRotatedHAndW maxYsym))

    let minY = (List.minBy (fun (x:Symbol) -> x.Pos.Y) symbols).Pos.Y

    {TopLeft = {X = minX; Y = minY}; W = maxX-minX; H = maxY-minY}


/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a rotation type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is rotated about</param>
/// <param name="rotation"> Clockwise or Anticlockwise </param>
/// <returns>New flipped point</returns>
let rotatePointAboutBlockCentre 
            (point:XYPos) 
            (centre:XYPos) 
            (rotation:Rotation) = 
    let relativeToCentre = (fun x-> x - centre)
    let rotateAboutCentre (pointIn:XYPos) = 
        match rotation with 
        | Degree0 -> 
            pointIn
        | Degree90 ->
            {X = pointIn.Y ; Y = -pointIn.X}
        | Degree180 -> 
            {X = -pointIn.X ; Y = - pointIn.Y}
        | Degree270 ->
            {X = -pointIn.Y ; Y = pointIn.X}
           
    let relativeToTopLeft = (fun x-> centre - x)

    point
    |> relativeToCentre
    |> rotateAboutCentre
    |> relativeToTopLeft

/// <summary>HLP 23: AUTHOR Ismagilov - Takes a point Pos, a centre Pos, and a flip type and returns the point flipped about the centre</summary>
/// <param name="point"> Original XYPos</param>
/// <param name="center"> The center XYPos that the point is flipped about</param>
/// <param name="flip"> Horizontal or Vertical flip</param>
/// <returns>New flipped point</returns>
let flipPointAboutBlockCentre 
    (point:XYPos)
    (center:XYPos)
    (flip:FlipType) = 
    match flip with
    | FlipHorizontal-> 
        {X = center.X - (point.X - center.X); Y = point.Y} 
    | FlipVertical -> 
        {X = point.X; Y = center.Y - (point.Y - center.Y)}

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been rotated</summary>
/// <param name="rotation"> Rotated CW or AntiCW</param>
/// <param name="h"> Original height of symbol (Before rotation)</param>
/// <param name="w"> Original width of symbol (Before rotation)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockRotation
        (rotation:Rotation) 
        (h: float)
        (w:float)
        (pos: XYPos)
         : XYPos =
    let posOffset =
        match rotation with
        | Degree0 -> {X = 0; Y = 0}
        | Degree90 -> {X=(float)h ;Y=0}
        | Degree180 -> {X= (float)w; Y= -(float)h}
        | Degree270 -> { X = 0 ;Y = (float)w }
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Get the new top left of a symbol after it has been flipped</summary>
/// <param name="flip">  Flipped horizontally or vertically</param>
/// <param name="h"> Original height of symbol (Before flip)</param>
/// <param name="w"> Original width of symbol (Before flip)</param>
/// <param name="sym"> Symbol</param>
/// <returns>New top left point of the symbol</returns>
let adjustPosForBlockFlip
        (flip:FlipType) 
        (h: float)
        (w:float)
        (pos: XYPos) =
    let posOffset =
        match flip with
        | FlipHorizontal -> {X=(float)w ;Y=0}
        | FlipVertical -> { X = 0 ;Y = (float)h }
    pos - posOffset

/// <summary>HLP 23: AUTHOR Ismagilov - Rotate a symbol in its block.</summary>
/// <param name="rotation">  Clockwise or Anticlockwise rotation</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after rotated about block centre.</returns>
let rotateSymbolInBlock 
        (rotation: Rotation) 
        (blockCentre: XYPos)
        (sym: Symbol)  : Symbol =
      
    let h,w = getRotatedHAndW sym

    let newTopLeft = 
        rotatePointAboutBlockCentre sym.Pos blockCentre (invertRotation rotation)
        |> adjustPosForBlockRotation (invertRotation rotation) h w

    let newComponent = { sym.Component with X = newTopLeft.X; Y = newTopLeft.Y}
    
    let newSTransform = 
        match sym.STransform.Flipped with
        | true -> 
            {sym.STransform with Rotation = combineRotation (invertRotation rotation) sym.STransform.Rotation}  
        | _-> 
            {sym.STransform with Rotation = combineRotation rotation sym.STransform.Rotation}

    { sym with 
        Pos = newTopLeft;
        PortMaps = rotatePortInfo rotation sym.PortMaps
        STransform = newSTransform 
        LabelHasDefaultPos = true
        Component = newComponent
    } |> calcLabelBoundingBox 




// bam21 start
/// <summary>HLP 23: AUTHOR Ismagilov - Flip a symbol horizontally or vertically in its block.</summary>
/// <param name="flip">  Flip horizontally or vertically</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be flipped</param>
/// <returns>New symbol after flipped about block centre.</returns>
let flipSymbolInBlock
    (flip: FlipType)
    (blockCentre: XYPos)
    (sym: Symbol) : Symbol =

    let h,w = getRotatedHAndW sym
    //Needed as new symbols and their components need their Pos updated (not done in regular flip symbol)
    
    // Changed: add pipeline
    let updatedTopLeft = 
        flip 
        |> flipPointAboutBlockCentre sym.Pos blockCentre
        |> adjustPosForBlockFlip flip h w
    

    // Changed: reformat pipeline to increase readability, renamed identifier
    let flippedPortOrientation = 
        sym.PortMaps.Orientation 
        |> Map.map (fun id side -> flipSideHorizontal side)

    // Changed: reformat pipeline to increase readability
    let flipPortList currPortOrder side =
        currPortOrder 
        |> Map.add (flipSideHorizontal side ) sym.PortMaps.Order[side]

    // Change: identifier
    let flippedPortOrder = 
        (Map.empty, [Edge.Top; Edge.Left; Edge.Bottom; Edge.Right]) ||> List.fold flipPortList
        |> Map.map (fun edge order -> List.rev order)   
  
    // Change: RSN (Reduce Syntactic Noise) by avoiding redundancy (defining sym.STransform.Rotation as itself)
    // and make it into one line
    let updatedSTransform = { sym.STransform with Flipped = not sym.STransform.Flipped }

    let updatedComponent = { sym.Component with X = updatedTopLeft.X; Y = updatedTopLeft.Y }

    // Change: Transform 3, broke pipeline because it was too long and defined variable locally. 
    // Improves DUI principle, by giving the block of code below an identifier.
    
    let updatedSym = 
        { sym with
            Component = updatedComponent;
            PortMaps = { Order = flippedPortOrder; Orientation = flippedPortOrientation };
            STransform = updatedSTransform;
            LabelHasDefaultPos = true;
            Pos = updatedTopLeft }
        |> calcLabelBoundingBox

    // Change: Functional abstraction, wrap code into a function and give it a proper identifier (DUI)
    let applyFlip flip updatedSym =
        match flip with
        | FlipHorizontal -> 
            updatedSym
        | FlipVertical ->
            let newBlock = getBlock [updatedSym]
            let newBlockCenter = newBlock.Centre()
            updatedSym
            |> rotateSymbolInBlock Degree270 newBlockCenter 
            |> rotateSymbolInBlock Degree270 newBlockCenter

    applyFlip flip updatedSym


/// <summary>HLP 23: AUTHOR Ismagilov - Scales selected symbol up or down.</summary>
/// <param name="scaleType"> Scale up or down. Scaling distance is constant</param>
/// <param name="block"> Bounding box of selected components</param>
/// <param name="sym"> Symbol to be rotated</param>
/// <returns>New symbol after scaled about block centre.</returns>
let scaleSymbolInBlock
    //(Mag: float)
    (scaleType: ScaleType)
    (block: BoundingBox)
    (sym: Symbol) : Symbol =

    let symCenter = getRotatedSymbolCentre sym

    //Get x and y proportion of symbol to block
    let xProp = (symCenter.X - block.TopLeft.X) / block.W
    let yProp = (symCenter.Y - block.TopLeft.Y) / block.H

    // DRY & LFM: Abstract scaling operation to reduce repetition and improve clarity.
    let scaleOffset = match scaleType with
                      | ScaleUp -> -5.0, 10.0
                      | ScaleDown -> 5.0, -10.0

    let newCenterX = block.TopLeft.X + fst scaleOffset + (block.W + snd scaleOffset) * xProp
    let newCenterY = block.TopLeft.Y + fst scaleOffset + (block.H + snd scaleOffset) * yProp
    let newCenter = { X = newCenterX; Y = newCenterY }

    let h,w = getRotatedHAndW sym
    let newPos = { X = newCenter.X - w / 2.0; Y = newCenter.Y - h / 2.0 }
    let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y}

    { sym with Pos = newPos; Component = newComponent; LabelHasDefaultPos = true }


// Change: Add XML documentation (DUI)
/// <summary> HLP 23: AUTHOR Klapper - Rotates a symbol based on a degree, including: ports and component parameters. </summary>
/// <param name="degree">The rotation degree (Degree0, Degree90, Degree180, Degree270).</param>
/// <param name="sym">The symbol to rotate.</param>
/// <returns>The rotated symbol, or the original symbol if Degree0 is specified.</returns>
let rotateSymbolByDegree (degree: Rotation) (sym:Symbol)  =
    // Change: Renamed Identifier to make it clear we are calculating the center of the symbol (DUI)
    let symbolCenterPos = 
        { X = sym.Component.X + sym.Component.W / 2.0; 
          Y = sym.Component.Y + sym.Component.H / 2.0 }
    
    match degree with
    | Degree0 -> sym
    | _ ->  rotateSymbolInBlock degree symbolCenterPos sym
    

/// <summary>HLP 23: AUTHOR Ismagilov - Rotates a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="rotation"> Type of rotation to do</param>
/// <returns>New rotated symbol model</returns>
let rotateBlock (compList:ComponentId list) (model:SymbolT.Model) (rotation:Rotation) = 
    let selectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let unselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    //Get block properties of selected symbols
    let block = getBlock selectedSymbols
    
    let invertedRotation = invertRotation rotation
    
    //Rotated symbols about the center
    let newSymbols = 
        List.map (fun x -> rotateSymbolInBlock invertedRotation (block.Centre()) x) selectedSymbols 

    // Change: Create new identifier updatedSymbols and use pipelines to split up operation (DUI, LFM)
    let updatedSymbols = 
        newSymbols
        |> List.map2 (fun x y -> (x,y)) compList
        |> Map.ofList
        |> Map.fold (fun acc k v -> Map.add k v acc) unselectedSymbols
    
    //return model with block of rotated selected symbols, and unselected symbols
    { model with Symbols = updatedSymbols }

// bam21 End



let oneCompBoundsBothEdges (selectedSymbols: Symbol list) = 
    let maxXSymCentre = 
            selectedSymbols
            |> List.maxBy (fun (x:Symbol) -> x.Pos.X + snd (getRotatedHAndW x)) 
            |> getRotatedSymbolCentre
    let minXSymCentre =
            selectedSymbols
            |> List.minBy (fun (x:Symbol) -> x.Pos.X)
            |> getRotatedSymbolCentre
    let maxYSymCentre = 
            selectedSymbols
            |> List.maxBy (fun (y:Symbol) -> y.Pos.Y+ fst (getRotatedHAndW y))
            |> getRotatedSymbolCentre
    let minYSymCentre =
            selectedSymbols
            |> List.minBy (fun (y:Symbol) -> y.Pos.Y)
            |> getRotatedSymbolCentre
    (maxXSymCentre.X = minXSymCentre.X) || (maxYSymCentre.Y = minYSymCentre.Y)
    

let findSelectedSymbols (compList: ComponentId list) (model: SymbolT.Model) = 
    List.map (fun x -> model.Symbols |> Map.find x) compList

let getScalingFactorAndOffsetCentre (min:float) (matchMin:float) (max:float) (matchMax:float) = 
    let scaleFact = 
        if min = max || matchMax <= matchMin then 1. 
        else (matchMin - matchMax) / (min - max)
    let offsetC = 
        if scaleFact = 1. then 0.
        else (matchMin - min * scaleFact) / (1.-scaleFact)
    (scaleFact, offsetC)

/// Return set of floats that define how a group of components is scaled
let getScalingFactorAndOffsetCentreGroup
    (matchBBMin:XYPos)
    (matchBBMax:XYPos)
    (selectedSymbols: Symbol list) : ((float * float) * (float * float)) = 
    //(compList: ComponentId list)
    //(model: SymbolT.Model)

    //let selectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList

    let maxXSym = 
            selectedSymbols
            |> List.maxBy (fun (x:Symbol) -> x.Pos.X + snd (getRotatedHAndW x)) 

    let oldMaxX = (maxXSym |> getRotatedSymbolCentre).X
    let newMaxX = matchBBMax.X - (snd (getRotatedHAndW maxXSym))/2.

    let minXSym =
            selectedSymbols
            |> List.minBy (fun (x:Symbol) -> x.Pos.X)

    let oldMinX = (minXSym |> getRotatedSymbolCentre).X
    let newMinX = matchBBMin.X + (snd (getRotatedHAndW minXSym))/2.
    
    let maxYSym = 
            selectedSymbols
            |> List.maxBy (fun (y:Symbol) -> y.Pos.Y+ fst (getRotatedHAndW y))

    let oldMaxY = (maxYSym |> getRotatedSymbolCentre).Y
    let newMaxY = matchBBMax.Y - (fst (getRotatedHAndW maxYSym))/2.

    let minYSym =
            selectedSymbols
            |> List.minBy (fun (y:Symbol) -> y.Pos.Y)

    let oldMinY = (minYSym |>  getRotatedSymbolCentre).Y
    let newMinY = matchBBMin.Y + (fst (getRotatedHAndW minYSym))/2.
    
    let xSC = getScalingFactorAndOffsetCentre oldMinX newMinX oldMaxX newMaxX
    let ySC = getScalingFactorAndOffsetCentre oldMinY newMinY oldMaxY newMaxY
    (xSC, ySC)

/// Alter position of one symbol as needed in a scaling operation
let scaleSymbol
        (xYSC: (float * float) * (float * float))
        (sym: Symbol)
        : Symbol = 
    let symCentre =  getRotatedSymbolCentre sym
    let translateFunc scaleFact offsetC coordinate = (coordinate - offsetC) * scaleFact + offsetC
    let xSC = fst xYSC
    let ySC = snd xYSC
    let newX = translateFunc (fst xSC) (snd xSC) symCentre.X
    let newY = translateFunc (fst ySC) (snd ySC) symCentre.Y

    let symCentreOffsetFromTopLeft = {X = (snd (getRotatedHAndW sym))/2.; Y = (fst (getRotatedHAndW sym))/2.}
    let newTopLeftPos = {X = newX; Y = newY} - symCentreOffsetFromTopLeft
    let newComp = {sym.Component with X = newTopLeftPos.X; Y = newTopLeftPos.Y}

    {sym with Pos = newTopLeftPos; Component = newComp; LabelHasDefaultPos = true}

/// Part of the rotate and scale code       
let groupNewSelectedSymsModel
    (compList:ComponentId list) 
    (model:SymbolT.Model) 
    (selectedSymbols: Symbol list)
    (modifySymbolFunc) = 

    //let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    // let block = getBlock SelectedSymbols
    // printfn "bbCentreX:%A" (block.Centre()).X

    // let newSymbols = List.map (modifySymbolFunc (block.Centre())) SelectedSymbols
    let newSymbols = List.map (modifySymbolFunc) selectedSymbols

    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}


/// <summary>HLP 23: AUTHOR Ismagilov - Flips a block of symbols, returning the new symbol model</summary>
/// <param name="compList"> List of ComponentId's of selected components</param>
/// <param name="model"> Current symbol model</param>
/// <param name="flip"> Type of flip to do</param>
/// <returns>New flipped symbol model</returns>
let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) = 
    //Similar structure to rotateBlock, easy to understand
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
    
    let block = getBlock SelectedSymbols
  
    let newSymbols = 
        List.map (fun x -> flipSymbolInBlock flip (block.Centre()) x ) SelectedSymbols

    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}

/// After every model update this updates the "scaling box" part of the model to be correctly
/// displayed based on whetehr multiple components are selected and if so what is their "box"
/// In addition to changing the model directly, cmd may contain messages that make further changes.
let postUpdateScalingBox (model:SheetT.Model, cmd) = 
    
    let symbolCmd (msg: SymbolT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet (SheetT.Wire (BusWireT.Symbol msg)))
    let sheetCmd (msg: SheetT.Msg) = Elmish.Cmd.ofMsg (ModelType.Msg.Sheet msg)

    if (model.SelectedComponents.Length < 2) then 
        match model.ScalingBox with 
        | None ->  model, cmd
        | _ -> {model with ScalingBox = None}, 
                [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                 sheetCmd SheetT.UpdateBoundingBoxes]
                |> List.append [cmd]
                |> Elmish.Cmd.batch
    else 
        let newBoxBound = 
            model.SelectedComponents
            |> List.map (fun id -> Map.find id model.Wire.Symbol.Symbols)
            |> getBlock
        match model.ScalingBox with 
        | Some value when value.ScalingBoxBound = newBoxBound -> model, cmd
        | _ -> 
            let topleft = newBoxBound.TopLeft
            let rotateDeg90OffSet: XYPos = {X = newBoxBound.W+57.; Y = (newBoxBound.H/2.)-12.5}
            let rotateDeg270OffSet: XYPos = {X = -69.5; Y = (newBoxBound.H/2.)-12.5}
            let buttonOffSet: XYPos = {X = newBoxBound.W + 47.5; Y = -47.5}
            let dummyPos = (topleft + buttonOffSet)

            let makeButton = SymbolUpdate.createAnnotation ThemeType.Colourful
            let buttonSym = {makeButton ScaleButton dummyPos with Pos = (topleft + buttonOffSet)}
            let makeRotateSym sym = {sym with Component = {sym.Component with H = 25.; W=25.}}
            let rotateDeg90Sym = 
                makeButton (RotateButton Degree90) (topleft + rotateDeg90OffSet)
                |> makeRotateSym
            let rotateDeg270Sym = 
                {makeButton (RotateButton Degree270) (topleft + rotateDeg270OffSet) 
                    with SymbolT.STransform = {Rotation=Degree90 ; Flipped=false}}
                |> makeRotateSym

            let newSymbolMap = model.Wire.Symbol.Symbols 
                                                        |> Map.add buttonSym.Id buttonSym 
                                                        |> Map.add rotateDeg270Sym.Id rotateDeg270Sym 
                                                        |> Map.add rotateDeg90Sym.Id rotateDeg90Sym
            let initScalingBox: SheetT.ScalingBox = {
                ScalingBoxBound = newBoxBound;
                ScaleButton = buttonSym;
                RotateDeg90Button = rotateDeg90Sym;
                RotateDeg270Button = rotateDeg270Sym;
                ButtonList = [buttonSym.Id; rotateDeg270Sym.Id; rotateDeg90Sym.Id];
            }
            let newCmd =
                match model.ScalingBox with
                | Some _ -> [symbolCmd (SymbolT.DeleteSymbols (model.ScalingBox.Value).ButtonList);
                             sheetCmd SheetT.UpdateBoundingBoxes]
                            |> List.append [cmd]
                            |> Elmish.Cmd.batch
                | None -> cmd
            model
            |> Optic.set SheetT.scalingBox_ (Some initScalingBox)
            |> Optic.set SheetT.symbols_ newSymbolMap, 
            newCmd

