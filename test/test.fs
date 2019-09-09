// Learn more about F# at http://fsharp.org

open Expecto

open WorkflowDemo.Workflow
open WorkflowDemo.Common

/// The possible instructions that can be in a workflow. Each case (except Pure) has a value that is the expected
/// argument of the instruction and the result the instruction should return. The Pure case just contains the expected
/// result.
type ExpectedInstruction<'a> =
    | SetControlState of state:string * unit
    | SetDeviceState of state:Device.State * unit
    | AddControlMsg of msg:string * unit
    | ClearControlMsgs of unit * unit
    | AddDeviceMsg of msg:string * unit
    | ClearDeviceMsgs of unit * unit
    | GetDeviceInput of prompt:string * string
    | CancelDeviceInput of unit * unit
    | Wait of duration:int<Free.seconds> * unit
    | WaitForData of minDataValue:int * int
    | ResetData of unit * unit
    | GetCurrentData of unit * int
    | Pure of 'a

/// A workflow interpreter that checks each workflow action against the list of expected actions that it is given.
/// Both the instruction type and argument are checked, then the result from the expected actions is the result of the
/// action.
let rec interpretTest expectedProgram program =    
    let expected = List.head expectedProgram
    let getExpectedTail () =
        let tail = List.tail expectedProgram
        Expect.isNonEmpty tail "Expecting there to be more program"
        tail
    match program with
    | Free.Pure x -> Expect.equal (Pure x) expected "Expected to get the correct final value"
    | Free.Free (Free.SetControlState (x, next)) ->
        match expected with
        | SetControlState (state, res) ->
            Expect.equal x state "Expected to get correct argument for SetControlState"            
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.SetDeviceState (x, next)) ->
        match expected with
        | SetDeviceState (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for SetDeviceState"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.AddControlMsg (x, next)) ->
        match expected with
        | AddControlMsg (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for AddControlMsg"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.ClearControlMsgs (x, next)) ->
        match expected with
        | ClearControlMsgs (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for ClearControlMsgs"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.AddDeviceMsg (x, next)) ->
        match expected with
        | AddDeviceMsg (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for AddDeviceMsg"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.ClearDeviceMsgs (x, next)) ->
        match expected with
        | ClearDeviceMsgs (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for ClearDeviceMsgs"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.GetDeviceInput (x, next)) ->
        match expected with
        | GetDeviceInput (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for GetDeviceInput"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.CancelDeviceInput (x, next)) ->
        match expected with
        | CancelDeviceInput (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for CancelDeviceInput"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.Wait (x, next)) ->
        match expected with
        | Wait (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for Wait"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.WaitForData (x, next)) ->
        match expected with
        | WaitForData (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for WaitForData"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.ResetData (x, next)) ->
        match expected with
        | ResetData (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for ResetData"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)
    | Free.Free (Free.GetCurrentData (x, next)) ->
        match expected with
        | GetCurrentData (arg, res) ->
            Expect.equal x arg "Expected to get correct argument for GetCurrentData"
            res |> next |> (getExpectedTail () |> interpretTest)
        | _ ->
            failtest (sprintf "Expected %A but got %A instead" expected program)

let tests =
    testList "Workflow" [
        
        testCase "wait" <| fun _ ->
            //TODO: Exercise 2: Fill in expected so that this test will test the wait workflow from exercise 1
            let expected = []
        
            interpretTest expected Workflows.wait            
    ]


[<EntryPoint>]
let main argv = runTestsWithArgs defaultConfig argv tests
