module WorkflowDemo.Workflow.Free

open WorkflowDemo.Common

[<Measure>]
type seconds

[<Measure>]
type ms

let msInSeconds = 1000<ms/seconds>

/// The workflow instructions. Each DU case is an instruction. The first member of the argument pair of an instruction
/// is the instructions argument (use unit for instructions that don't have arguments). The second member of the pair
/// is a function that takes the result type of the instruction (use unit if the instruction has no result) and returns
/// the type 'a. To add a new instruction just add a case to this DU, then update all the broken match expressions and
/// add a convenience function for the instruction below.
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
    
/// map function that turns WorkflowInstruction into a functor. Every case follows the same pattern.
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
    
/// Combines workflow instructions into programs (builds a monad on top of the WorkflowInstruction functor)
type WorkflowProgram<'a> =
| Free of WorkflowInstruction<WorkflowProgram<'a>>
| Pure of 'a    

/// Allows workflow programs to be combined.
let rec bind f = function
| Free x -> x |> mapI (bind f) |> Free
| Pure x -> f x

/// Builds the workflow computation expression. 
type WorkflowBuilder () =
    member this.Bind (x, f) = bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero () = Pure ()
    
/// The workflow computation expression. Use the functions below within this computation expression to build
/// workflow programs. These programs can then be passed to interpreters to be run.
let workflow = WorkflowBuilder ()

/// Folds the given sequence into a workflow program over the elements of the sequence. I.e. it calls the given function
/// for each element of the sequence. For the first sequence element the given initial state is also passed to the
/// function. Subsequent calls of the function will use the previous result of the function for the state argument. The
/// result of the fold will be the result of the final call of the function. 
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
    
// These are convenience functions for use in the workflow computation expression.
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

/// Test interpreter.
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
