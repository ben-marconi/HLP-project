module TestDrawBlockD3
// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.BusWireT
open DrawModelType.SheetT
open DrawModelType.SymbolT
open EEExtensions
open Elmish
open GenerateData
open Helpers
open ModelType
open Optics
open Optics.Operators
open Sheet.SheetInterface
open SheetBeautifyHelpers
open SheetUpdateHelpers
open SymbolResizeHelpers
open TestDrawBlock
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests
open TestLib


//--------------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------------Helper Functions-----------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

// Function to print any value
let print x = printfn "%A" x

/// <summary> Prints metrics related to wire intersections and right angles within a given sheet model. </summary>
/// <param name="model">The sheet model to analyze.</param>
let printMetrics (model : SheetT.Model) : unit =
    // T1
    let symbolPairIntersection : int = numOfIntersectedSymPairs model
    print ($"Number of Symbol and Symbol Intersections: {symbolPairIntersection}")

    // T2
    let segmentSymbolIntersections : int = numOfIntersectSegSym model
    print ($"Number of Symbol and Wire Intersections: {segmentSymbolIntersections}")

    // T3
    let wireIntersections : int = numOfWireRightAngleCrossings model
    print ($"Number of Wire Intersections: {wireIntersections}")

    // T4
    let totalSegLength : float = calcVisWireLength model
    print ($"Total Segment Length: {totalSegLength}")

    // T5
    let wireRightAngles : int = numOfVisRightAngles model
    print ($"Number of Wire Right angles: {wireRightAngles}")

let countWireLabelIntersections (sheet:SheetT.Model) =
    let wires = sheet.Wire.Wires |> Map.toList
    let labels = sheet.Wire.Symbol.Symbols
                 |> Map.filter (fun _ sym -> sym.Component.Type = IOLabel)
    let labelBoxes = labels
                     |> Map.map (fun _ sym -> getSymBoundingBox sym)
                     |> Map.toList
                     |> List.map (fun (_, x) -> x)
    List.allPairs wires labelBoxes
    |> List.map (fun (w,bbox) ->
            let folder pos1 pos2 acc seg =
                match BlockHelpers.overlap2D (pos1,pos2) (bbox.TopLeft,bbox.BottomRight()) with
                | true -> acc + 1
                | false -> acc
            BlockHelpers.foldOverNonZeroSegs folder 0 (snd w))
    |> List.sum


/// <summary> Finds the ID of a symbol within a model by its label. </summary>
/// <param name="model">The sheet model containing the symbols.</param>
/// <param name="targetLabel">The label of the symbol to find.</param>
/// <returns> The ID of the symbol if found, wrapped in an Option type; otherwise, None. </returns>
let findSymbolIdByLabel (model: SheetT.Model) (targetLabel: string) : Option<ComponentId> =
    model.Wire.Symbol.Symbols
    |> Map.toSeq
    |> Seq.tryFind (fun (_, sym) -> sym.Component.Label.ToUpper() = targetLabel.ToUpper())
    |> Option.map fst



// Placeholder for actual implementation
let beautify (model : SheetT.Model) : SheetT.Model =
    model

/// <summary> Beautifies the given sheet within the model then updates and displays the new sheet. Prints metrics before and after changes. </summary>
/// <param name="model">The model containing the sheet to be beautified.</param>
/// <param name="dispatch"> dispatch</param>
let beautifySheet (model : ModelType.Model) (dispatch: Dispatch<Msg>): unit =
    // Use optic to get sheet
    let sheet = Optic.get sheet_ model

    // Print metrics for current Sheet
    print "Metrics before beautifying"
    printMetrics sheet

    // Beautify sheet and print new metrics
    let newSheet = beautify sheet
    print "Metrics after beautifying"
    printMetrics newSheet

    // Update the Model with the new sheet and display it
    let newModel = Optic.set sheet_ newSheet model
    showSheetInIssieSchematic newModel.Sheet dispatch

//--------------------------------------------------------------------------------------------------------------------------//
//--------------------------------------------------Test Circuit Generators-------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
let makeLabelTest1Circuit labelPosition =
    initSheetModel
    |> placeSymbol "G1" (GateN(And,2)) (middleOfSheet + {X = -100; Y = 0})
    |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
    |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
    |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
    |> Result.bind (placeSymbol "LBL1" IOLabel labelPosition)
    |> getOkOrFail
    |> beautify

let makeLabelTest2Circuit muxDistance =
    initSheetModel
    |> placeSymbol "DMUX1" (Demux4) (middleOfSheet + muxDistance)
    |> Result.bind (placeSymbol "MUX1" Mux4 middleOfSheet)
    |> Result.bind (placeWire (portOf "DMUX1" 0) (portOf "MUX1" 0))
    |> Result.bind (placeWire (portOf "DMUX1" 1) (portOf "MUX1" 1))
    |> Result.bind (placeWire (portOf "DMUX1" 2) (portOf "MUX1" 2))
    |> Result.bind (placeWire (portOf "DMUX1" 3) (portOf "MUX1" 3))
    |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "LBL1" 0))
    |> getOkOrFail
    |> beautify

//---------------------------------------------------------------------------------------------------------------------------//
//---------------------------------------------------D3 Custom Asserts-------------------------------------------------------//
//---------------------------------------------------------------------------------------------------------------------------//


let failOnLabelWireIntersection (sample:int) model =
    let intersections = countWireLabelIntersections model
    if intersections = 0 then
        None
    else
        Some $"Wire intersects a wire label in Sample {sample}, at least {intersections} intersections"

let failOnOversizeWires (sample:int) model : string option =
    failwithf "Not implemented"



//--------------------------------------------------------------------------------------------------------------------------//
//------------------------------------------------D3 tests on Draw Block code-----------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//
let test1 testNum firstSample dispatch =
    failwithf "Not implemented"
    // runTestOnSheets
    //     "2 MUXes and 1 Gate With Random Flips"
    //     firstSample
    //     //makeTest1Circuit
    //     //FlipAndRotate
    //     failOnAllTests
    //     dispatch
    // |> recordPositionInTest testNum dispatch



let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
    // Change names and test functions as required
    // delete unused tests from list
    [
        "Test1", fun _ _ _ -> printf "Test1" // dummy test - delete line or replace by real test as needed
        "Test2", fun _ _ _ -> printf "Test2" // dummy test - delete line or replace by real test as needed
        "Test3", fun _ _ _ -> printf "Test3" // dummy test - delete line or replace by real test as needed
        "Test4", fun _ _ _ -> printf "Test4" // dummy test - delete line or replace by real test as needed
        "Test5", fun _ _ _ -> printf "Test5" // dummy test - delete line or replace by real test as needed
        "Test6", fun _ _ _ -> printf "Test6"
        "Test7", fun _ _ _ -> printf "Test7"
        "Run Beautify Function", fun _ _ _ -> printf "Running beautify algorithm"
        "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test

    ]

let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex]
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | "Run Beautify Function", _ ->
                printf "Test Finished"
                beautifySheet model dispatch
                ()
            | _ ->
                func testIndex 0 dispatch
