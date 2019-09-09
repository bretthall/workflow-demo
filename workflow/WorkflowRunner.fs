module WorkflowDemo.Workflow.WorkflowRunner

open System.Threading
open System

open WorkflowDemo.Workflow
open WorkflowDemo.Common

module private Internal = 
    
    // Used to capture the results of workflow actions so that the workflow can be played back later.
    type WorkflowResult =
        | SetControlState of unit
        | SetDeviceState of unit
        | AddControlMsg of unit
        | ClearControlMsgs of unit
        | AddDeviceMsg of unit
        | ClearDeviceMsgs of unit
        | GetDeviceInput of string
        | CancelDeviceInput of unit
        | WaitStart of start:DateTime * duration:int<Free.seconds>
        | Wait of unit
        | WaitForData of int
        | ResetData of unit
        //Exercise 6: Added case for ResetDataWithMsg
        | ResetDataWithMsg of unit
        | GetCurrentData of int
        | Pause of start:DateTime

    type WorkflowResultMsg =
        | NewResult of WorkflowResult * AsyncReplyChannel<unit>
        | Resume of resumeTime:DateTime * AsyncReplyChannel<unit>
        
    type ActionMsg =
        | SetControlState of state:string * AsyncReplyChannel<unit>
        | SetDeviceState of state:Device.State * AsyncReplyChannel<unit>
        | AddControlMsg of msg:string * AsyncReplyChannel<unit> 
        | ClearControlMsgs of AsyncReplyChannel<unit>
        | AddDeviceMsg of msg:string * AsyncReplyChannel<unit> 
        | ClearDeviceMsgs of AsyncReplyChannel<unit>
        | GetDeviceInput of prompt:string * AsyncReplyChannel<string>
        | CancelDeviceInput of AsyncReplyChannel<unit> 
        | Wait of startTime:DateTime * duration:int<Free.seconds> * AsyncReplyChannel<unit>
        | WaitForData of minDataValue:int * AsyncReplyChannel<int>
        | ResetData of AsyncReplyChannel<unit>
        | GetCurrentData of AsyncReplyChannel<int>

    type DeviceMsg =
        | DataUpdate of int
        | InputReceived of string
        
    type RunnerAgentMsg =
        | Action of ActionMsg
        | Device of DeviceMsg
        | TimerDone of int
        | Pause of startTime:DateTime
        | Resume
        | Finish
        | Cancel

    type WaitingForTimeState = {
        reply: AsyncReplyChannel<unit>
        timerIndex: int
        cancelTimer: CancellationTokenSource
        duration: TimeSpan
        start: DateTime
    }
    
    type WaitingForDataState = {
        reply: AsyncReplyChannel<int>
        minDataValue: int
    }
    
    type WorkflowState =
        | RunningCommands
        | WaitingForInput of AsyncReplyChannel<string>
        | WaitingForTime of WaitingForTimeState
        | WaitingForData of WaitingForDataState
        | Paused of PausedState
        | Done
    and PausedState = {
        start: DateTime
        nextAction: Option<ActionMsg>
        input: Option<string>
        previousState: WorkflowState
    }
        
    type State = {
        workflowState: WorkflowState
        dataValue: int
    }
        
open Internal

/// Messages sent on the Msgs event of Runner.
type RunnerMsg =
    | SetControlState of state:string 
    | SetDeviceState of state:Device.State
    | AddControlMsg of msg:string 
    | ClearControlMsgs
    | AddDeviceMsg of msg:string 
    | ClearDeviceMsgs 
    | StartDeviceInput of prompt:string 
    | CancelDeviceInput 
    | ResetData
    | Paused
    | Resumed
    | Cancelled
    | Finished
    
/// Captures the current state of a Runner. Can be used to resume a workflow that was stopped.
type RunnerState = private RunnerState of List<WorkflowResult>

/// An empty RunnerState, use this for fresh workflow runs that aren't continuations of a workflow that was stopped.
let defaultRunnerState = RunnerState []

/// Executes the given workflow program. If this is the resumption of a cancelled workflow run then prevRunnerState
/// should be the last saved state passed to saveState during the previous run, defaultRunnerState should be passed
/// if this is a fresh workflow run. The most recent data from the instrument should be passed in curData. saveState
/// wil be called every time the runner state is updated as actions are executed. onInit will be called after the runner
/// is setup but before it starts executing the workflow. 
type Runner(program: Free.WorkflowProgram<unit>,
            curData: int,
            prevRunnerState: RunnerState,
            saveState: (RunnerState -> Async<unit>),
            onInit: Runner -> unit) as this =
    
    let msgs = Event<RunnerMsg> ()
    
    let saveLog = NLog.LogManager.GetLogger "Runner.Save"
    
    // Due to workflow results arriving from two sources (the workflow interpreter and the pause/resume methods)
    // we serialize them through this mailbox processor.
    let saveAgent = MailboxProcessor<WorkflowResultMsg>.Start (fun inbox ->
        
        inbox.Error.Add (fun err -> saveLog.Error (sprintf "Save agent got error: %A" err))
        
        let rec handler results =
            async {
                match! inbox.Receive () with
                | WorkflowResultMsg.NewResult (result, reply) ->
                    saveLog.Info (sprintf "Got new result: %A" result)
                    // There's a chance that a pause can arrive before an action message. The pause should always be on
                    // the end of the list so pull any pause that is currently on the list off here and save it for
                    // later
                    let curPause, results =
                        match List.tryHead results with
                        | Some (WorkflowResult.Pause startTime) -> Some (WorkflowResult.Pause startTime), List.tail results
                        | _ -> None, results                                                
                    // We also need to handle Wait special since it has two stages: WaitStart and Wait. When we get
                    // the second stage (Wait) we need to throw away the associated WaitStart since the wait is now done.
                    let results =
                        match result with
                        | WorkflowResult.Pause _ ->
                            match curPause with
                            | None -> result :: results
                            | _ -> failwith "Got pause when already paused"
                        | WorkflowResult.Wait () ->
                            match List.tryHead results with
                            | Some (WorkflowResult.WaitStart _) ->
                                result :: (List.tail results)
                            | _ ->
                                result :: results
                        | _ ->
                            result :: results 
                    // if we were paused then stick the pause result back ont he list here.
                    let results =
                        match curPause with
                        | Some pause -> pause :: results
                        | None -> results                        
                    do! saveState (RunnerState results)
                    saveLog.Info (sprintf "Saved results: %A" results)
                    reply.Reply ()
                    return! handler results
                | WorkflowResultMsg.Resume (resumeTime, reply) ->
                    saveLog.Info "Got resume"
                    // If we're paused then there will be a pause at the head of the list that needs to be removed
                    let results = 
                        match List.tryHead results with
                        | Some (WorkflowResult.Pause pauseTime) ->
                            // if we paused while waiting then we need to adjust the WaitStart result for the amount
                            // of time that we were paused.
                            let results = List.tail results
                            match List.tryHead results with
                            | Some (WaitStart (startTime, duration)) ->
                                let newStart = startTime.Add (resumeTime - pauseTime)
                                (WorkflowResult.WaitStart (newStart, duration) :: (List.tail results))
                            | _ ->
                                results
                        | _ ->
                            failwith "Got resume without corresponding pause"
                    do! saveState (RunnerState results)
                    saveLog.Info (sprintf "Saved results after resume: %A" results)
                    reply.Reply ()
                    return! handler results
            }
        handler []
    )

    let log = NLog.LogManager.GetLogger "Runner"
    
    // this mailbox processor handles multiplexing the execution of workflow actions and handling pause resume,
    // receiving data and input from the device, and workflow cancellation.
    let workflowAgent = MailboxProcessor<RunnerAgentMsg>.Start(fun inbox ->
        
        inbox.Error.Add (fun err -> log.Error (sprintf "Agent got error: %A" err))
        
        let mutable timerIndex = 0
        let startTimer (duration: TimeSpan) =
            let index = timerIndex
            timerIndex <- timerIndex + 1
            let comp = async {
                log.Info (sprintf "Started timer %d with duration %A" index duration)
                do! Async.Sleep (int duration.TotalMilliseconds)
                log.Info (sprintf "Timer %d done" index)
                inbox.Post (TimerDone index)
            }
            let cancel = new CancellationTokenSource ()
            Async.Start (comp, cancel.Token)
            cancel, index
            
        let updateData curData msg =
            match msg with
            | Device dMsg ->
                match dMsg with
                | DeviceMsg.DataUpdate value -> value
                | _ -> curData 
            | _ -> curData 
            
        let logMessage state msg =
            match msg with
            | RunnerAgentMsg.Device d ->
                match d with
                | DeviceMsg.DataUpdate _ -> ()
                | _ -> log.Info (sprintf "Got message in %A with state %A" msg state)
            | _ -> log.Info (sprintf "Got message in %A with state %A" msg state)
                
        let rec waitForMessage state =
            async {
                let! msg = inbox.Receive()
                logMessage state msg
                return! waitForMessage (handleMessage state msg)
            }
        and handleMessage state msg =
            let newState = {state with dataValue = updateData state.dataValue msg}
            match msg with
            | RunnerAgentMsg.Pause startTime ->
                match newState.workflowState with
                | WorkflowState.Paused _ | WorkflowState.Done -> newState
                | _ ->
                    match newState.workflowState with
                    | WorkflowState.WaitingForTime timeState  ->
                        timeState.cancelTimer.Cancel ()
                    | _ -> ()
                    saveAgent.PostAndReply (fun r -> WorkflowResultMsg.NewResult (WorkflowResult.Pause startTime, r))
                    msgs.Trigger RunnerMsg.Paused
                    {newState with
                        workflowState = WorkflowState.Paused {
                            start = startTime
                            nextAction = None
                            input = None
                            previousState  = state.workflowState
                        }
                    }
            | RunnerAgentMsg.Cancel ->
                msgs.Trigger (RunnerMsg.Cancelled)
                {newState with
                    workflowState = Done
                }
            | RunnerAgentMsg.Finish  ->
                msgs.Trigger RunnerMsg.Finished
                {newState with 
                    workflowState = Done
                }
            | _ ->
                match state.workflowState with
                | WorkflowState.RunningCommands -> runningCmds newState msg
                | WorkflowState.WaitingForInput reply -> waitingForInput newState reply msg
                | WorkflowState.WaitingForTime timeState -> waitingForTime newState timeState msg
                | WorkflowState.WaitingForData dataState -> waitingForData newState dataState msg
                | WorkflowState.Paused pauseState -> paused newState pauseState msg
                | WorkflowState.Done -> newState
        and runningCmds state msg =
            match msg with
            | Action action ->
                match action with
                | ActionMsg.SetControlState (controlState, reply) ->
                    msgs.Trigger (RunnerMsg.SetControlState controlState)
                    reply.Reply ()
                    state
                | ActionMsg.SetDeviceState (deviceState, reply) ->
                    msgs.Trigger (RunnerMsg.SetDeviceState deviceState)
                    reply.Reply ()
                    state
                | ActionMsg.AddControlMsg (m, reply) ->
                    msgs.Trigger (RunnerMsg.AddControlMsg m)
                    reply.Reply ()
                    state
                | ActionMsg.ClearControlMsgs reply ->
                    msgs.Trigger RunnerMsg.ClearControlMsgs
                    reply.Reply ()
                    state
                | ActionMsg.AddDeviceMsg (m, reply) ->
                    msgs.Trigger (RunnerMsg.AddDeviceMsg m)
                    reply.Reply ()
                    state
                | ActionMsg.ClearDeviceMsgs reply ->
                    msgs.Trigger RunnerMsg.ClearDeviceMsgs
                    reply.Reply ()
                    state
                | ActionMsg.GetDeviceInput (prompt, reply) ->
                    msgs.Trigger (RunnerMsg.StartDeviceInput prompt)
                    {state with workflowState = WaitingForInput reply}
                | ActionMsg.CancelDeviceInput reply ->
                    msgs.Trigger (RunnerMsg.CancelDeviceInput)
                    reply.Reply ()
                    state
                | ActionMsg.Wait (startTime, duration, reply) ->
                    let now = DateTime.Now
                    let waitDuration = (int duration |> float |> TimeSpan.FromSeconds) - (now - startTime)
                    if waitDuration.TotalSeconds > 0.0 then
                        let cancel, index = startTimer waitDuration
                        let waitState = {
                            reply = reply
                            cancelTimer = cancel
                            timerIndex = index
                            duration = waitDuration
                            start = startTime
                        }
                        {state with workflowState = WaitingForTime waitState}
                    else
                        reply.Reply ()
                        {state with workflowState = RunningCommands}
                | ActionMsg.WaitForData (minDataValue, reply) ->
                    let waitState = {
                        reply = reply
                        minDataValue = minDataValue
                    }
                    {state with workflowState = WaitingForData waitState}
                | ActionMsg.ResetData reply ->
                    msgs.Trigger RunnerMsg.ResetData
                    reply.Reply ()
                    state
                | ActionMsg.GetCurrentData reply ->
                    reply.Reply state.dataValue
                    state
            | _ -> state                
        and waitingForInput state reply msg =
            match msg with
            | Action a ->
                failwith (sprintf "Got action %A while waiting for input" a)
            | Device d ->
                match d with
                | InputReceived input ->
                    reply.Reply input
                    {state with workflowState = RunningCommands}
                | _ ->
                    state
            | _ ->
                state
        and waitingForTime state timeState msg =
            match msg with
            | Action a ->
                failwith (sprintf "Got action %A while waiting for time" a)
            | TimerDone index ->
                if index = timeState.timerIndex then
                    timeState.reply.Reply ()
                    {state with workflowState = RunningCommands}
                else
                    state
            | _ -> 
                state
        and waitingForData state dataState msg =
            match msg with
            | Action a ->
                failwith (sprintf "Got action %A while waiting for data" a)                
            | Device dMsg ->
                match dMsg with
                | DataUpdate value ->
                    if value >= dataState.minDataValue then
                        dataState.reply.Reply value
                        {state with workflowState = RunningCommands}
                    else
                        state
                | _ ->
                    state
            | _ -> 
                state
        and paused state (pauseState: PausedState) msg =
            match msg with
            | Action a ->
                match pauseState.nextAction with
                | None ->
                    {state with workflowState = WorkflowState.Paused {pauseState with nextAction = Some a}}
                | Some _ -> 
                    failwith (sprintf "Got second action %A while paused" a)
            | Device d ->
                match d with
                | DeviceMsg.InputReceived input -> {state with workflowState = WorkflowState.Paused {pauseState with input = Some input}}
                | _ -> state
            | Resume ->
                handleResume state pauseState
            | _ ->
                state
        and handleResume state pauseState =
            let now = DateTime.Now
            saveAgent.PostAndReply (fun r -> WorkflowResultMsg.Resume (now, r))
            let newState = 
                match pauseState.previousState with
                | WaitingForTime timeState ->
                    // If we were doing a timed wait when the pause happened then we need to adjust the
                    // timed wait information so that the time we were paused isn't counted.
                    let newStart = timeState.start.Add (now - pauseState.start)
                    let diff = now - newStart
                    let cancel, index = startTimer (timeState.duration - diff)
                    {state with
                        workflowState =
                            WaitingForTime {
                                timeState with
                                    timerIndex = index
                                    cancelTimer = cancel
                                    start = newStart
                    }}
                | WaitingForData dataState ->                    
                    let newState = {state with workflowState = pauseState.previousState}
                    waitingForData newState dataState (DeviceMsg.DataUpdate state.dataValue |> Device)
                | WaitingForInput inputState ->
                    match pauseState.input with
                    | Some input ->
                        let newState = {state with workflowState = pauseState.previousState}
                        waitingForInput newState inputState (DeviceMsg.InputReceived input |> Device)
                    | None ->
                        {state with workflowState = pauseState.previousState}                    
                | _ ->
                    {state with workflowState = pauseState.previousState}
            match pauseState.nextAction with
            | None -> newState
            | Some action -> handleMessage newState (Action action)                
        waitForMessage {
            workflowState = RunningCommands
            dataValue = curData
        }
    )
    
    let interpLog = NLog.LogManager.GetLogger "Runner.Interp"
        
    let saveResults prevResults newResult =
        async {
            do! saveAgent.PostAndAsyncReply (fun r -> WorkflowResultMsg.NewResult (newResult, r))
            return newResult :: prevResults
        }
        
    // The interpreter for the workflow. Dispatches actions to the workflowAgent. This function is called recursively
    // to process the actions in the workflow. As long as the prevResults argument is not empty the actions are handled
    // using the contents of prevResults. curResults starts empty and is then used to capture the results of the actions
    // that are run so that they can be used as the prevResults if the workflow is stopped and then resumed.
    let rec interpret prevResults curResults program =
        async {
            match prevResults with
            | (WorkflowResult.Pause startTime) :: [] ->
                // The workflow was paused when it was stopped so make sure that we pause it here
                workflowAgent.Post (RunnerAgentMsg.Pause startTime)
                return! interpretNoResults curResults program
            | (WorkflowResult.Pause _) :: rest ->
                // A pause must be a the end of prevResults    
                failwith (sprintf "Got pause with other results following it: %A" rest)
            | _ :: _ ->
                // there are still previous results so continue processing them
                return! interpretWithResults prevResults curResults program
            | [] ->
                // prevResults are exhausted so now run the rest of the workflow for real.
                interpLog.Info (sprintf "Out of results, switching to interpretNoResults. curResults = %A" curResults)
                return! interpretNoResults curResults program                
        }
    // Simulates workflow execution using prevResults that were captured during a previous workflow run.
    and interpretWithResults prevResults curResults program =
        async {
            match program with
            | Free.Pure x ->
                interpLog.Info (sprintf "saved Pure %A" x)
                return x
            | Free.Free (Free.SetControlState (x, next)) ->
                match prevResults with
                | (WorkflowResult.SetControlState res) :: rest ->
                    interpLog.Info (sprintf "SetControlState %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.SetControlState res)
                    return! res |> next |> interpret rest newResults
                | result :: _ -> failwith (sprintf "Was expecting SetControlState saved result but got %A" result)
                | [] -> failwith "Was expecting SetControlState saved result but got no result"
            | Free.Free (Free.SetDeviceState (x, next)) -> 
                match prevResults with
                | (WorkflowResult.SetDeviceState res) :: rest ->
                    interpLog.Info (sprintf "SetDeviceState %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.SetDeviceState res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting SetDeviceState saved result but got %A" result)
                | [] -> failwith "Was expecting SetDeviceState saved result but got no result"
            | Free.Free (Free.AddControlMsg (x, next)) -> 
                match prevResults with
                | (WorkflowResult.AddControlMsg res) :: rest ->
                    interpLog.Info (sprintf "AddControlMsg %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.AddControlMsg res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting AddControlMsg saved result but got %A" result)
                | [] -> failwith "Was expecting AddControlMsg saved result but got no result"
            | Free.Free (Free.ClearControlMsgs (x, next)) -> 
                match prevResults with
                | (WorkflowResult.ClearControlMsgs res) :: rest ->
                    interpLog.Info (sprintf "ClearControlMsgs %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.ClearControlMsgs res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting ClearControlMsgs saved result but got %A" result)
                | [] -> failwith "Was expecting ClearControlMsgs saved result but got no result"
            | Free.Free (Free.AddDeviceMsg (x, next)) -> 
                match prevResults with
                | (WorkflowResult.AddDeviceMsg res) :: rest ->
                    interpLog.Info (sprintf "AddDeviceMsg %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.AddDeviceMsg res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting AddDeviceMsg saved result but got %A" result)
                | [] -> failwith "Was expecting AddDeviceMsg saved result but got no result"
            | Free.Free (Free.ClearDeviceMsgs (x, next)) -> 
                match prevResults with
                | (WorkflowResult.ClearDeviceMsgs res) :: rest ->
                    interpLog.Info (sprintf "ClearDeviceMsgs %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.ClearDeviceMsgs res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting ClearDeviceMsgs saved result but got %A" result)
                | [] -> failwith "Was expecting ClearDeviceMsgs saved result but got no result"
            | Free.Free (Free.GetDeviceInput (x, next)) -> 
                match prevResults with
                | (WorkflowResult.GetDeviceInput res) :: rest ->
                    interpLog.Info (sprintf "GetDeviceInput %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.GetDeviceInput res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting GetDeviceInput saved result but got %A" result)
                | [] -> failwith "Was expecting GetDeviceInput saved result but got no result"
            | Free.Free (Free.CancelDeviceInput (x, next)) -> 
                match prevResults with
                | (WorkflowResult.CancelDeviceInput res) :: rest ->
                    interpLog.Info (sprintf "CancelDeviceInput %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.CancelDeviceInput res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting CancelDeviceInput saved result but got %A" result)
                | [] -> failwith "Was expecting CancelDeviceInput saved result but got no result"
            | Free.Free (Free.Wait (x, next)) -> 
                // Due to adjustments that have to be made due to timing we have to handle the Wait action specially
                match prevResults with
                | (WorkflowResult.WaitStart (startTime, duration)) :: rest ->
                    // check to see if the workflow was paused during the wait
                    match rest with
                    | (WorkflowResult.Pause pauseTime) :: [] -> 
                        interpLog.Info (sprintf "Paused saved result: %A" pauseTime)
                        workflowAgent.Post (RunnerAgentMsg.Pause pauseTime)
                    | [] ->
                        // a wait start can only be followed by a pause in prevResults, if the wait ended then
                        // we should have a Wait instead (which can be followed by other results) 
                        interpLog.Info "no results after wait start"                        
                    | _ -> 
                        failwith (sprintf "Got wait start with other results following it: %A" rest)
                    interpLog.Info (sprintf "Wait start %A saved result: %A" x (startTime, duration))
                    let! _ = saveResults curResults (WorkflowResult.WaitStart (startTime, duration))
                    do! workflowAgent.PostAndAsyncReply (fun r  -> RunnerAgentMsg.Action (ActionMsg.Wait (startTime, duration, r)))
                    let! newResults = saveResults curResults (WorkflowResult.Wait ())
                    return! () |> next |> interpret rest newResults 
                | (WorkflowResult.Wait res) :: rest ->
                    // The wait in the previous workflow run completed so continue on to the next action
                    interpLog.Info (sprintf "Wait %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.Wait res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting Wait saved result but got %A" result)
                | [] -> failwith "Was expecting Wait saved result but got no result"
            | Free.Free (Free.WaitForData (x, next)) -> 
                match prevResults with
                | (WorkflowResult.WaitForData res) :: rest ->
                    interpLog.Info (sprintf "WaitForData %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.WaitForData res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting WaitForData saved result but got %A" result)
                | [] -> failwith "Was expecting WaitForData saved result but got no result"
            | Free.Free (Free.ResetData (x, next)) -> 
                match prevResults with
                | (WorkflowResult.ResetData res) :: rest ->
                    interpLog.Info (sprintf "ResetData %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.ResetData res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting ResetData saved result but got %A" result)
                | [] -> failwith "Was expecting ResetData saved result but got no result"
            //Exercise 6: Added handler for ResetDataWithMsg
            | Free.Free (Free.ResetDataWithMsg (x, next)) -> 
                match prevResults with
                | (WorkflowResult.ResetDataWithMsg res) :: rest ->
                    interpLog.Info (sprintf "ResetDataWithMsg %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.ResetDataWithMsg res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting ResetDataWithMsg saved result but got %A" result)
                | [] -> failwith "Was expecting ResetDataWithMsg saved result but got no result"
            | Free.Free (Free.GetCurrentData (x, next)) -> 
                match prevResults with
                | (WorkflowResult.GetCurrentData res) :: rest ->
                    interpLog.Info (sprintf "GetCurrentData %A saved result: %A" x res)
                    let! newResults = saveResults curResults (WorkflowResult.GetCurrentData res)
                    return! res |> next |> interpret rest newResults 
                | result :: _ -> failwith (sprintf "Was expecting GetCurrentData saved result but got %A" result)
                | [] -> failwith "Was expecting GetCurrentData saved result but got no result"
        }
    // Runs the remaining workflow actions for real after we've run out of results from a previous run 
    and interpretNoResults results program =
        async {
            match program with
            | Free.Pure x ->
                interpLog.Info (sprintf "Pure %A" x)
                //Don't bother saving state here, workflow is done
                return x
            | Free.Free (Free.SetControlState (state, next)) ->
                interpLog.Info (sprintf "SetControlState %A" state)
                let! res =  workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.SetControlState (state, r)))
                interpLog.Info (sprintf "SetControlState %A result: %A" state res)
                let! newResults = saveResults results (WorkflowResult.SetControlState ())
                return! res |> next |> interpretNoResults newResults
            | Free.Free (Free.SetDeviceState (state, next)) -> 
                interpLog.Info (sprintf "SetDeviceState %A" state)
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.SetDeviceState (state, r)))
                interpLog.Info (sprintf "SetControlState %A done" state)
                let! newResults = saveResults results (WorkflowResult.SetDeviceState ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.AddControlMsg (msg, next)) -> 
                interpLog.Info (sprintf "AddControlMsg %A" msg)
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.AddControlMsg (msg, r)))
                interpLog.Info (sprintf "AddControlMsg %A done" msg)
                let! newResults = saveResults results (WorkflowResult.AddControlMsg ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.ClearControlMsgs ((), next)) -> 
                interpLog.Info "ClearControlMsgs"
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.ClearControlMsgs r))
                interpLog.Info "ClearControlMsgs done"
                let! newResults = saveResults results (WorkflowResult.ClearControlMsgs ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.AddDeviceMsg (msg, next)) -> 
                interpLog.Info (sprintf "AddDeviceMsg %A" msg)
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.AddDeviceMsg (msg, r)))
                interpLog.Info (sprintf "AddDeviceMsg %A done" msg)
                let! newResults = saveResults results (WorkflowResult.AddDeviceMsg ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.ClearDeviceMsgs ((), next)) -> 
                interpLog.Info "ClearDeviceMsgs"
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.ClearDeviceMsgs r))
                interpLog.Info "ClearDeviceMsgs done"
                let! newResults = saveResults results (WorkflowResult.ClearDeviceMsgs ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.GetDeviceInput (prompt, next)) -> 
                interpLog.Info (sprintf "GetDeviceInput %A" prompt)
                let! input = workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.GetDeviceInput (prompt, r)))
                interpLog.Info (sprintf "GetDeviceInput %A result: %s" prompt input)
                let! newResults = saveResults results (WorkflowResult.GetDeviceInput input)
                return! input |> next |> interpretNoResults newResults
            | Free.Free (Free.CancelDeviceInput ((), next)) -> 
                interpLog.Info "CancelDeviceInput"
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.CancelDeviceInput r))
                interpLog.Info "CancelDeviceInput done"
                let! newResults = saveResults results (WorkflowResult.CancelDeviceInput ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.Wait (duration, next)) ->
                // Wait gets special handling so that workflow resumption will work properly: we capture a
                // WaitStart result first with out start time and when the wait completes we capture a Wait result
                // which replaces the WaitStart (see saveResults)
                interpLog.Info (sprintf "Wait %A" duration)
                let startTime = DateTime.Now
                let! _ = saveResults results (WorkflowResult.WaitStart (startTime, duration))
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.Wait (startTime, duration, r)))
                interpLog.Info (sprintf "Wait %A done" duration)
                let! newResults = saveResults results (WorkflowResult.Wait ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.WaitForData (minDataValue, next)) ->
                interpLog.Info (sprintf "Wait %A" minDataValue)
                let! value = workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.WaitForData (minDataValue, r)))
                interpLog.Info (sprintf "Wait %A done" minDataValue)
                let! newResults = saveResults results (WorkflowResult.WaitForData value)
                return! value |> next |> interpretNoResults newResults 
            | Free.Free (Free.ResetData ((), next)) -> 
                interpLog.Info "ResetData"
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.ResetData r))
                interpLog.Info "ResetData done"
                let! newResults = saveResults results (WorkflowResult.ResetData ())
                return! () |> next |> interpretNoResults newResults
            //Exercise 6: Added handler for ResetDataWithMsg
            | Free.Free (Free.ResetDataWithMsg ((), next)) -> 
                interpLog.Info "ResetDataWithMsg"
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.ResetData r))
                do! workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.AddDeviceMsg ("reset data", r)))
                interpLog.Info "ResetDataWithMsg done"
                let! newResults = saveResults results (WorkflowResult.ResetDataWithMsg ())
                return! () |> next |> interpretNoResults newResults
            | Free.Free (Free.GetCurrentData ((), next)) -> 
                interpLog.Info "GetCurrentData"
                let! value = workflowAgent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.GetCurrentData r))
                interpLog.Info (sprintf "GetCurrentData result: %A" value)
                let! newResults = saveResults results (WorkflowResult.GetCurrentData value) 
                return! value |> next |> interpretNoResults newResults    
        }

    let programCancel = new CancellationTokenSource ()
    
    do
        // This section starts the workflow running
        let (RunnerState prevResults) = prevRunnerState
        let runProgram = async {
            log.Info "Calling onInit"
            onInit this
            log.Info (sprintf "Starting workflow with prevResults: %A" prevResults)
            do! interpret (List.rev prevResults) [] program
            log.Info "Workflow done"
            workflowAgent.Post RunnerAgentMsg.Finish
        }    
        do Async.Start (runProgram, programCancel.Token)

    /// Pauses the workflow
    member __.Pause () = workflowAgent.Post (Pause DateTime.Now)
    /// Resumes a paused workflow
    member __.Resume () = workflowAgent.Post Resume
    
    /// Cancels the workflow
    member __.Cancel () =
        log.Info "Cancelled"
        programCancel.Cancel ()
        workflowAgent.Post (RunnerAgentMsg.Cancel)
    
    /// Call this when data arrives from the device
    member __.DataUpdate value = workflowAgent.Post (DataUpdate value |> Device)
    /// Call this when input arrives from the device
    member __.InputReceived input = workflowAgent.Post (InputReceived input |> Device)
    
    /// Event for monitoring workflow progress
    member __.Msgs = msgs.Publish