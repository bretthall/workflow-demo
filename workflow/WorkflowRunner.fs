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
        previousState: WorkflowState
    }
        
    type State = {
        workflowState: WorkflowState
        lastDataValue: int
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
    
type Runner(program: Free.WorkflowProgram<unit>) =
    
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
                
        let rec loop state =
            async {
                let! msg = inbox.Receive()
                logMessage state msg
                let curData = updateData state.lastDataValue msg
                match msg with
                | RunnerAgentMsg.Pause ->
                    match state.workflowState with
                    | WorkflowState.Paused _ | WorkflowState.Done -> return! loop state
                    | _ ->
                        match state.workflowState with
                        | WorkflowState.WaitingForTime timeState  ->
                            timeState.cancelTimer.Cancel ()
                        | _ -> ()
                        return! loop {
                            workflowState = WorkflowState.Paused {
                                start = DateTime.Now
                                previousState  = state.workflowState
                            }
                            lastDataValue = state.lastDataValue
                        }
                | RunnerAgentMsg.Cancel ->
                    msgs.Trigger (RunnerMsg.Cancelled)
                    return! loop {
                        workflowState = Done
                        lastDataValue = state.lastDataValue
                    }
                | RunnerAgentMsg.Finish  ->
                    msgs.Trigger RunnerMsg.Finished
                    return! loop {
                        workflowState = Done
                        lastDataValue = state.lastDataValue
                    }
                | _ ->
                    let newWorkflowState =
                        match state.workflowState with
                        | WorkflowState.RunningCommands -> runningCmds curData msg
                        | WorkflowState.WaitingForInput reply -> waitingForInput reply msg
                        | WorkflowState.WaitingForTime timeState -> waitingForTime timeState msg
                        | WorkflowState.WaitingForData dataState -> waitingForData dataState msg
                        | WorkflowState.Paused pauseState -> paused pauseState msg
                        | WorkflowState.Done -> state.workflowState
                    let newState = {workflowState = newWorkflowState; lastDataValue = curData}
                    return! loop newState
            }
        and runningCmds curData msg =
            match msg with
            | Action action ->
                match action with
                | ActionMsg.SetControlState (state, reply) ->
                    msgs.Trigger (RunnerMsg.SetControlState state)
                    reply.Reply ()
                    RunningCommands
                | ActionMsg.SetDeviceState (state, reply) ->
                    msgs.Trigger (RunnerMsg.SetDeviceState state)
                    reply.Reply ()
                    RunningCommands
                | ActionMsg.AddControlMsg (m, reply) ->
                    msgs.Trigger (RunnerMsg.AddControlMsg m)
                    reply.Reply ()
                    RunningCommands
                | ActionMsg.ClearControlMsgs reply ->
                    msgs.Trigger RunnerMsg.ClearControlMsgs
                    reply.Reply ()
                    RunningCommands
                | ActionMsg.AddDeviceMsg (m, reply) ->
                    msgs.Trigger (RunnerMsg.AddDeviceMsg m)
                    reply.Reply ()
                    RunningCommands
                | ActionMsg.ClearDeviceMsgs reply ->
                    msgs.Trigger RunnerMsg.ClearDeviceMsgs
                    reply.Reply ()
                    RunningCommands
                | ActionMsg.GetDeviceInput (prompt, reply) ->
                    msgs.Trigger (RunnerMsg.StartDeviceInput prompt)
                    WaitingForInput reply
                | ActionMsg.CancelDeviceInput reply ->
                    msgs.Trigger (RunnerMsg.CancelDeviceInput)
                    reply.Reply ()
                    RunningCommands
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
                    WaitingForTime waitState
                | ActionMsg.WaitForData (minDataValue, reply) ->
                    let waitState = {
                        reply = reply
                        minDataValue = minDataValue
                    }
                    WaitingForData waitState
                | ActionMsg.ResetData reply ->
                    msgs.Trigger RunnerMsg.ResetData
                    reply.Reply ()
                    RunningCommands
                | ActionMsg.GetCurrentData reply ->
                    reply.Reply curData
                    RunningCommands
            | _ -> RunningCommands                
        and waitingForInput reply msg =
            match msg with
            | Action a ->
                failwith (sprintf "Got action %A while waiting for input" a)
            | Device d ->
                match d with
                | InputReceived input ->
                    reply.Reply input
                    RunningCommands
                | _ ->
                    WaitingForInput reply
            | _ ->
                WaitingForInput reply
        and waitingForTime timeState msg =
            match msg with
            | Action a ->
                failwith (sprintf "Got action %A while waiting for time" a)
            | TimerDone index ->
                if index = timeState.timerIndex then
                    timeState.reply.Reply ()
                    RunningCommands
                else
                    WaitingForTime timeState
            | _ -> 
                WaitingForTime timeState
        and waitingForData dataState msg =
            match msg with
            | Action a ->
                failwith (sprintf "Got action %A while waiting for data" a)                
            | Device dMsg ->
                match dMsg with
                | DataUpdate value ->
                    if value >= dataState.minDataValue then
                        dataState.reply.Reply value
                        RunningCommands
                    else
                        WaitingForData dataState
                | _ ->
                    WaitingForData dataState
            | _ -> 
                WaitingForData dataState
        and paused (pauseState: PausedState) msg =
            match msg with
            | Action a ->
                failwith (sprintf "Got action %A while paused" a)                
            | Resume ->
                match pauseState.previousState with
                | WaitingForTime timeState ->
                    let now = DateTime.Now
                    let newStart = timeState.start.Add (now - pauseState.start)
                    let diff = now - newStart
                    let cancel, index = startTimer (timeState.duration - diff)
                    WaitingForTime {
                        timeState with
                            timerIndex = index
                            cancelTimer = cancel
                            start = newStart
                    }
                | _ ->
                    pauseState.previousState
            | _ ->
                WorkflowState.Paused pauseState
        loop {
            workflowState = RunningCommands
            lastDataValue = 0
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