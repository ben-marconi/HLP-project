module SheetBeautifyHelpers

open CommonTypes.JSONComponent
open DrawModelType.SymbolT
open CommonTypes
open Optics
open Optics.Optic

//B1R
let getCustomSymbolDimension (sym:Symbol) =
    let comp = get component_ sym
    (Option.defaultValue 1.0 sym.HScale * get w_ comp,
     Option.defaultValue 1.0 sym.VScale * get h_ comp)

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

let getSymbolPortsOnSide side (sym:Symbol) =
    let pMap = get portMaps_ sym
    get order_ pMap
    |> Map.tryFind side

let setSymbolPortsOnSide side ports sym =
    let pMap = get portMaps_ sym
    let orderMap = get order_ pMap
                   |> Map.add side ports
    set portMaps_ (set order_ orderMap pMap) sym

