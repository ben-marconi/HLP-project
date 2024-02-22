module SheetBeautifyHelpers

open CommonTypes.JSONComponent
open DrawModelType
open DrawModelType.SymbolT
open CommonTypes
open Optics
open Optics.Optic

//B1R
let getCustomSymbolDimension (sym:Symbol) =
    let comp = get component_ sym
    let scale = get scaleF_ sym
    (scale.X * get w_ comp,
     scale.Y * get h_ comp)

//B1W
let setCustomSymbolDimension (width, height) (sym:Symbol) =
    let comp = get component_ sym
    let newComp = set w_ width comp
                |> set h_ height
    set component_ comp sym

/// Returns the dimension of a symbol as a tuple (width * height)
let customSymbolDimension_ = Lens.create (getCustomSymbolDimension) (setCustomSymbolDimension)

//B2W
let setSymbolPosition (position:XYPos) (sym:Symbol) =
    set posOfSym_ position sym
//B3R
let getSymbolPortsOnSide side (sym:Symbol) =
    let pMap = get portMaps_ sym
    get order_ pMap
    |> Map.tryFind side
//B3W
let setSymbolPortsOnSide side ports sym =
    let pMap = get portMaps_ sym
    let orderMap = get order_ pMap
                   |> Map.add side ports
    set portMaps_ (set order_ orderMap pMap) sym

//B4R B4W
let reversedInputPorts_ = Lens.create (fun (a:Symbol) -> a.ReversedInputPorts) (fun s a -> {a with ReversedInputPorts = s})

//B5R
//let getPortPosition (port:PortId) (sheet:SheetInfo) =

//B6R
let getBoundingBoxOfSymbol (model:SheetT.Model) (sym:Symbol) =
    // This exists in Symbol.getSymbolBoundingBox which is probably a better function
    let bb = model.BoundingBoxes
    Map.tryFind sym.Id bb


let stransform_ = Lens.create (fun (a:Symbol) -> a.STransform) (fun s a -> {a with STransform = s})
//B7R
let getSymbolRotation (sym:Symbol) =
    (get stransform_ sym).Rotation
//B7W
let setSymbolRotation (rotation:Rotation) (sym:Symbol) =
    SymbolResizeHelpers.rotateSymbol rotation sym
//B8R
let getSymbolFlip (sym:Symbol) =
    (get stransform_ sym).Flipped
//B8W
let setSymbolFlip (flip:FlipType) (sym:Symbol) =
    SymbolResizeHelpers.flipSymbol flip sym

//T1R
let countSymbolIntersections (sheet: SheetT.Model) =
    let boxes =
        Helpers.mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length

//T2R
let countVisibleWireIntersections (sheet: SheetT.Model) =
    failwithf "Not Implemented"
