module TestDrawBlockD1
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
    SheetBeautifyD1.sheetAlignScale model
    // model

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
//-----------------------------------------------------Test Circuit---------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------------//

let makeTestCircuit1 (beautify: bool) =
    initSheetModel
    |> placeSymbol "MUX1" (Mux2) (middleOfSheet + { X = -150; Y = -90 })
    |> Result.bind (placeSymbol "MUX2" (Mux2) (middleOfSheet))
    |> Result.bind (placeSymbol "G1" (GateN (And,2)) (middleOfSheet + { X = 150; Y = -76 }))
    |> Result.bind (placeSymbol "G2" (GateN (Or,2)) (middleOfSheet + { X = 150; Y = 71.5 }))
    |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
    |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1))
    |> Result.bind (placeWire (portOf "G1" 0) (portOf "G2" 1))
    |> (fun res -> if beautify then Result.map SheetBeautifyD1.sheetAlignScale res else res)
    |> getOkOrFail

let makeTestCircuitA2 (beautify: bool) =
    initSheetModel
    |> placeSymbol "MUX1" (Mux2) (middleOfSheet + { X = -150; Y = -90 })
    |> Result.bind (placeSymbol "MUX2" (Mux2) (middleOfSheet))
    |> Result.bind (placeSymbol "A" (Input1 (1, None)) (middleOfSheet + { X = -350; Y = -130 }))
    |> Result.bind (placeSymbol "B" (Input1 (1, None)) (middleOfSheet + { X = -350; Y = -50 }))
    |> Result.bind (placeSymbol "C" (Output 1) (middleOfSheet + { X = 150; Y = -76 }))
    |> Result.bind (placeSymbol "S1" (Input1 (1, None)) (middleOfSheet + { X = -140; Y = 176 }))
    |> Result.bind (placeSymbol "S2" (Input1 (1, None)) (middleOfSheet + { X = -230; Y = 100 }))
    |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
    |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
    |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
    |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
    |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> (fun res -> if beautify then Result.map SheetBeautifyD1.sheetAlignScale res else res)
    |> getOkOrFail


let test1 testNum firstSample dispatch =
    runTestOnSheets
        "2 muxes single connected and 2 gates connected"
        firstSample
        (fromList [ false; true ])
        makeTestCircuit1
        Asserts.failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch
let test2 testNum firstSample dispatch =
    runTestOnSheets
        "2 muxes and Viewers connected as per example Fig A1"
        firstSample
        (fromList [ false; true ])
        makeTestCircuitA2
        Asserts.failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch

let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
    // Change names and test functions as required
    // delete unused tests from list
    [
        "Test1", test1 // dummy test - delete line or replace by real test as needed
        "Test2", test2 // dummy test - delete line or replace by real test as needed
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