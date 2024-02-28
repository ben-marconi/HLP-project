module TestDrawBlockT2
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
open TestDrawBlock
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests
open TestLib


// D2 Testing

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//


// Define the positions of Symbols in the Sheet
let mux1PosCircuit1 : XYPos = middleOfSheet - { X = 150.0; Y = 175.0 }
let mux2PosCircuit1 : XYPos = middleOfSheet
let input1PosCircuit1 : XYPos = middleOfSheet - { X = 200.0; Y = -28.0 }
let input2PosCircuit1 : XYPos = middleOfSheet - { X = 200.0; Y = -90.0 }
let andPosCircuit1 : XYPos = middleOfSheet - { X = -15.0; Y = 20.0 }

let makeCircuit1 (andPos:XYPos) =
    initSheetModel
    |> placeSymbol "G1" (GateN(And, 2)) (andPos - {X= -200.0; Y=14.0})
    |> Result.bind (placeSymbol "MUX1" Mux2 mux1PosCircuit1)
    |> Result.bind (placeSymbol "MUX2" Mux2 mux2PosCircuit1)
    |> Result.bind (placeSymbol "S1" (Input1(1, Some 1)) input1PosCircuit1)
    |> Result.bind (placeSymbol "S2" (Input1(1, Some 1)) input2PosCircuit1)
    |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
    |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0))
    |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
    |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> getOkOrFail

// let makeCircuit2


let test1 testNum firstSample dispatch =
    runTestOnSheets
        "2 MUXes With Random Flips"
        firstSample
        horizLinePositions
        makeCircuit1 
        failOnAllTests
        dispatch
    |> recordPositionInTest testNum dispatch






let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
    // Change names and test functions as required
    // delete unused tests from list
    [
        "Test1", test1
        "Test2", fun _ _ _ -> printf "Test2" // dummy test - delete line or replace by real test as needed
        "Test3", fun _ _ _ -> printf "Test3" // dummy test - delete line or replace by real test as needed
        "Test4", fun _ _ _ -> printf "Test4" // dummy test - delete line or replace by real test as needed
        "Test5", fun _ _ _ -> printf "Test5" // dummy test - delete line or replace by real test as needed
        "Test6", fun _ _ _ -> printf "Test6"
        "Test7", fun _ _ _ -> printf "Test7"
        "Test8", fun _ _ _ -> printf "Test8"
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
            | _ ->
                func testIndex 0 dispatch
