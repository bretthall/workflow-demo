module WorkflowDemo.Workflow.WorkflowRunner

open System.Threading
open System

open WorkflowDemo.Workflow
open WorkflowDemo.Common

module private Internal = 
    
    type ActionMsg =
        | SetControlState of state:string * AsyncReplyChannel<unit>
        | SetDeviceState of state:Device.State * AsyncReplyChannel<unit>
        | AddControlMsg of msg:string * AsyncReplyChannel<unit> 
        | ClearControlMsgs of AsyncReplyChannel<unit>
        | AddDeviceMsg of msg:string * AsyncReplyChannel<unit> 
        | ClearDeviceMsgs of AsyncReplyChannel<unit>
        | GetDeviceInput of prompt:string * AsyncReplyChannel<string>
        | CancelDeviceInput of AsyncReplyChannel<unit> 
        | Wait of duration:int<Free.seconds> * AsyncReplyChannel<unit>
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
        | Pause
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
    
type Runner(program: Free.WorkflowProgram<unit>, curData: int) =
    
    let log = NLog.LogManager.GetLogger "Runner"
    
    let msgs = Event<RunnerMsg> ()
    
    let agent = MailboxProcessor<RunnerAgentMsg>.Start(fun inbox ->
        
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
            | RunnerAgentMsg.Pause ->
                match newState.workflowState with
                | WorkflowState.Paused _ | WorkflowState.Done -> newState
                | _ ->
                    match newState.workflowState with
                    | WorkflowState.WaitingForTime timeState  ->
                        timeState.cancelTimer.Cancel ()
                    | _ -> ()
                    {newState with
                        workflowState = WorkflowState.Paused {
                            start = DateTime.Now
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
                | ActionMsg.Wait (duration, reply) ->
                    let now = DateTime.Now
                    let waitDuration = (int duration |> float |> TimeSpan.FromSeconds)
                    let cancel, index = startTimer waitDuration
                    let waitState = {
                        reply = reply
                        cancelTimer = cancel
                        timerIndex = index
                        duration = waitDuration
                        start = now
                    }
                    {state with workflowState = WaitingForTime waitState}
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
            let newState = 
                match pauseState.previousState with
                | WaitingForTime timeState ->
                    let now = DateTime.Now
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
    
    let rec interpret program =
        async {
            match program with
            | Free.Pure x ->
                interpLog.Info (sprintf "Pure %A" x)
                return x
            | Free.Free (Free.SetControlState (state, next)) ->
                interpLog.Info (sprintf "SetControlState %A" state)
                let! res =  agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.SetControlState (state, r)))
                interpLog.Info (sprintf "SetControlState %A result: %A" state res)
                return! res |> next |> interpret
            | Free.Free (Free.SetDeviceState (state, next)) -> 
                interpLog.Info (sprintf "SetDeviceState %A" state)
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.SetDeviceState (state, r)))
                interpLog.Info (sprintf "SetControlState %A done" state)
                return! () |> next |> interpret
            | Free.Free (Free.AddControlMsg (msg, next)) -> 
                interpLog.Info (sprintf "AddControlMsg %A" msg)
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.AddControlMsg (msg, r)))
                interpLog.Info (sprintf "AddControlMsg %A done" msg)
                return! () |> next |> interpret
            | Free.Free (Free.ClearControlMsgs ((), next)) -> 
                interpLog.Info "ClearControlMsgs"
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.ClearControlMsgs r))
                interpLog.Info "ClearControlMsgs done"
                return! () |> next |> interpret
            | Free.Free (Free.AddDeviceMsg (msg, next)) -> 
                interpLog.Info (sprintf "AddDeviceMsg %A" msg)
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.AddDeviceMsg (msg, r)))
                interpLog.Info (sprintf "AddDeviceMsg %A done" msg)
                return! () |> next |> interpret
            | Free.Free (Free.ClearDeviceMsgs ((), next)) -> 
                interpLog.Info "ClearDeviceMsgs"
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.ClearDeviceMsgs r))
                interpLog.Info "ClearDeviceMsgs done"
                return! () |> next |> interpret
            | Free.Free (Free.GetDeviceInput (prompt, next)) -> 
                interpLog.Info (sprintf "GetDeviceInput %A" prompt)
                let! input = agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.GetDeviceInput (prompt, r)))
                interpLog.Info (sprintf "GetDeviceInput %A result: %s" prompt input)
                return! input |> next |> interpret
            | Free.Free (Free.CancelDeviceInput ((), next)) -> 
                interpLog.Info "CancelDeviceInput"
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.CancelDeviceInput r))
                interpLog.Info "CancelDeviceInput done"
                return! () |> next |> interpret
            | Free.Free (Free.Wait (duration, next)) ->
                interpLog.Info (sprintf "Wait %A" duration)
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.Wait (duration, r)))
                interpLog.Info (sprintf "Wait %A done" duration)
                return! () |> next |> interpret
            | Free.Free (Free.WaitForData (minDataValue, next)) ->
                interpLog.Info (sprintf "Wait %A" minDataValue)
                let! value = agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.WaitForData (minDataValue, r)))
                interpLog.Info (sprintf "Wait %A done" minDataValue)
                return! value |> next |> interpret                
            | Free.Free (Free.ResetData ((), next)) -> 
                interpLog.Info "ResetData"
                do! agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.ResetData r))
                interpLog.Info "ResetData done"
                return! () |> next |> interpret
            | Free.Free (Free.GetCurrentData ((), next)) -> 
                interpLog.Info "GetCurrentData"
                let! value = agent.PostAndAsyncReply (fun r -> RunnerAgentMsg.Action (ActionMsg.GetCurrentData r))
                interpLog.Info (sprintf "GetCurrentData result: %A" value)
                return! value |> next |> interpret                    
        }

    let programCancel = new CancellationTokenSource ()
    
    member __.Start () =
        let runProgram = async {
            log.Info "Starting workflow"
            do! interpret program
            log.Info "Workflow done"
            agent.Post RunnerAgentMsg.Finish
        }    
        do Async.Start (runProgram, programCancel.Token)

    member __.Pause () = agent.Post Pause
    member __.Resume () = agent.Post Resume
    
    member __.Cancel () =
        log.Info "Cancelled"
        programCancel.Cancel ()
        agent.Post (RunnerAgentMsg.Cancel)
    
    member __.DataUpdate value = agent.Post (DataUpdate value |> Device)
    member __.InputReceived input = agent.Post (InputReceived input |> Device)
    
    member __.Msgs = msgs.Publish