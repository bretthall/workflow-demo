// Learn more about F# at http://fsharp.org

open Expecto

open WorkflowDemo.Workflow
open WorkflowDemo.Common

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
            
            let expected = [
                yield SetDeviceState (Device.Good, ())
                yield AddDeviceMsg ("Waiting for 5 seconds", ())
                yield Wait (5<Free.seconds>, ())
                yield SetDeviceState (Device.Bad, ())
                let curData = 5
                yield GetCurrentData ((), curData)
                let targetData = curData + 5
                yield AddDeviceMsg ((sprintf "Waiting for data = %d" targetData), ())
                let finalData = targetData + 1
                yield WaitForData (targetData, finalData)
                yield AddDeviceMsg ((sprintf "final data = %d" finalData), ())
                yield SetDeviceState (Device.Ugly, ())
                yield AddDeviceMsg ("Waiting for input", ())
                let input = "testing 1 2 3"
                yield GetDeviceInput ("Waiting for input", input)
                yield AddDeviceMsg ((sprintf "Got input = %s" input), ())
                yield SetDeviceState (Device.Good, ())
                yield Pure ()
            ]
        
            interpretTest expected Workflows.wait            
    ]


[<EntryPoint>]
let main argv = runTestsWithArgs defaultConfig argv tests
