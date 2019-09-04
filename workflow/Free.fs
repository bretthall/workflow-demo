module WorkflowDemo.Workflow.Free

open WorkflowDemo.Common

[<Measure>]
type seconds

[<Measure>]
type ms

let msInSeconds = 1000<ms/seconds>

type WorkflowInstruction<'a> =
    | SetControlState of state:string * (unit -> 'a)
    | SetDeviceState of state:Device.State * (unit -> 'a)
    | AddControlMsg of msg:string * (unit -> 'a)
    | ClearControlMsgs of unit * (unit -> 'a)
    | AddDeviceMsg of msg:string * (unit -> 'a)
    | ClearDeviceMsgs of unit * (unit -> 'a)
    | GetDeviceInput of prompt:string * (string -> 'a)
    | CancelDeviceInput of unit * (unit -> 'a)
    | Wait of duration:int<seconds> * (unit -> 'a)
    | WaitForData of minDataValue:int * (int -> 'a)
    | ResetData of unit * (unit -> 'a)
    | GetCurrentData of unit * (int -> 'a)
    
let private mapI f = function
    | SetControlState (x, next) -> SetControlState (x, next >> f)
    | SetDeviceState (x, next) -> SetDeviceState (x, next >> f)
    | AddControlMsg (x, next) -> AddControlMsg (x, next >> f)
    | ClearControlMsgs (x, next) -> ClearControlMsgs (x, next >> f)
    | AddDeviceMsg (x, next) -> AddDeviceMsg (x, next >> f)
    | ClearDeviceMsgs (x, next) -> ClearDeviceMsgs (x, next >> f)
    | GetDeviceInput (x, next) -> GetDeviceInput (x, next >> f)
    | CancelDeviceInput (x, next) -> CancelDeviceInput (x, next >> f)
    | Wait (x, next) -> Wait (x, next >> f)
    | WaitForData (x, next) -> WaitForData (x, next >> f)
    | ResetData (x, next) -> ResetData (x, next >> f)
    | GetCurrentData (x, next) -> GetCurrentData (x, next >> f)
    
type WorkflowProgram<'a> =
| Free of WorkflowInstruction<WorkflowProgram<'a>>
| Pure of 'a    

let rec bind f = function
| Free x -> x |> mapI (bind f) |> Free
| Pure x -> f x

type WorkflowBuilder () =
    member this.Bind (x, f) = bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero () = Pure ()
    
let workflow = WorkflowBuilder ()

let fold (f: 'State -> 'T -> WorkflowProgram<'State>) (init:'State) (values: seq<'T>) =
    let rec step state remaining =
        workflow {
            if Seq.isEmpty remaining then
                return state
            else
                let! newState = f state (Seq.head remaining)    
                return! step newState (Seq.tail remaining)
        }
    step init values
    
let setControlState state = Free (SetControlState (state, Pure))
let setDeviceState state = Free (SetDeviceState (state, Pure))
let addControlMsg msg = Free (AddControlMsg (msg, Pure))
let clearControlMsgs msg = Free (ClearControlMsgs (msg, Pure))
let addDeviceMsg msg = Free (AddDeviceMsg (msg, Pure))
let clearDeviceMsgs msg = Free (ClearDeviceMsgs (msg, Pure))
let getDeviceInput prompt = Free (GetDeviceInput (prompt, Pure))
let cancelDeviceInput () = Free (CancelDeviceInput ((), Pure))
let wait duration = Free (Wait (duration, Pure))
let waitForData minDataValue = Free (WaitForData (minDataValue, Pure))
let resetData () = Free (ResetData ((), Pure))
let getCurrentData () = Free (GetCurrentData ((), Pure))

let rec interpretTest (send: string -> unit) program =
    let interpret = interpretTest send
    let sendMessage res name value =
        send (sprintf "%s %A" name value)
        res
    match program with
    | Pure x -> x
    | Free (SetControlState (x, next)) -> x |> sendMessage () "SetControlState" |> next |> interpret
    | Free (SetDeviceState (x, next)) -> x |> sendMessage () "SetDeviceState"  |> next |> interpret
    | Free (AddControlMsg (x, next)) -> x |> sendMessage () "AddControlMsg"  |> next |> interpret
    | Free (ClearControlMsgs (x, next)) -> x |> sendMessage () "ClearControlMsgs"  |> next |> interpret
    | Free (AddDeviceMsg (x, next)) -> x |> sendMessage () "AddDeviceMsg"  |> next |> interpret
    | Free (ClearDeviceMsgs (x, next)) -> x |> sendMessage () "ClearDeviceMsgs"  |> next |> interpret
    | Free (GetDeviceInput (x, next)) -> x |> sendMessage "input" "GetDeviceInput"  |> next |> interpret
    | Free (CancelDeviceInput (x, next)) -> x |> sendMessage () "CancelDeviceInput"  |> next |> interpret
    | Free (Wait (x, next)) -> x |> sendMessage () "Wait"  |> next |> interpret
    | Free (WaitForData (x, next)) -> x |> sendMessage 10 "WaitForData"  |> next |> interpret
    | Free (ResetData (x, next)) -> x |> sendMessage () "ResetData"  |> next |> interpret
    | Free (GetCurrentData (x, next)) -> x |> sendMessage 1 "GetCurrentData"  |> next |> interpret
