module WorkflowDemo.Control.Main


open System
open Terminal.Gui
open Terminal.Gui.Elmish

open WorkflowDemo.Common
open WorkflowDemo.Workflow

type Client = PipeClient.Client<Device.Msg, Control.Msg>

let workflowToText wf =
    match wf with
    | Workflows.Reset -> "Reset Device"
    | Workflows.Test -> "Test"
    
let workflows =
    [
        Workflows.Reset
        Workflows.Test
    ] |> List.map (fun wf -> (wf, workflowToText wf))

type NotRunningState = {
    selectedWorkflow: Workflows.Workflow
}

type RunningState = {
    workflow: Workflows.Workflow
    runner: WorkflowRunner.Runner
    state: string
    paused: bool
    lastDataValue: int
    msgs: List<string>
}

type State =
    | NotRunning of NotRunningState
    | Running of RunningState
    | Done of RunningState
    
type Model = {
    client: Client
    state: State
}

let init client =
    let state = {
        client = client
        state = NotRunning {selectedWorkflow = Workflows.Reset}
    }
    state, Cmd.none

type WorkflowMsg =
    | Start
    | Pause of bool
    | Stop
    | Finish
    | Reset
    
type Msg =
    | ControlMsg of Control.Msg
    | WorkflowSelected of Workflows.Workflow
    | WorkflowMsg of WorkflowMsg
    | SetState of string
    | AddMsg of string
    | ClearMsgs
    | Quit

let handleRunnerMsgs (client: Client) dispatch msg =
    match msg with
    | WorkflowRunner.SetControlState state -> Application.MainLoop.Invoke (Action (fun _ -> dispatch (SetState state))) 
    | WorkflowRunner.SetDeviceState state -> client.Send (Device.SetState state) 
    | WorkflowRunner.AddControlMsg msg -> Application.MainLoop.Invoke (Action (fun _ -> dispatch (AddMsg msg)))
    | WorkflowRunner.ClearControlMsgs -> Application.MainLoop.Invoke (Action (fun _ -> dispatch ClearMsgs))
    | WorkflowRunner.AddDeviceMsg msg -> client.Send (Device.AddMsg msg)
    | WorkflowRunner.ClearDeviceMsgs -> client.Send Device.ClearMsgs
    | WorkflowRunner.StartDeviceInput prompt -> client.Send (Device.RequestInput prompt) 
    | WorkflowRunner.CancelDeviceInput -> client.Send Device.CancelInput
    | WorkflowRunner.ResetData -> client.Send Device.ResetData
    | WorkflowRunner.Paused -> ()
    | WorkflowRunner.Resumed -> ()
    | WorkflowRunner.Cancelled -> ()
    | WorkflowRunner.Finished -> Application.MainLoop.Invoke (Action (fun _ -> dispatch (WorkflowMsg Finish)))

let updateNotRunning client (state: NotRunningState) msg =
    match msg with
    | ControlMsg _ -> NotRunning state, Cmd.none
    | WorkflowSelected wf -> NotRunning {state with selectedWorkflow = wf}, Cmd.none
    | WorkflowMsg m ->
        match m with
        | Start ->
            let runner = WorkflowRunner.Runner (Workflows.getProgram state.selectedWorkflow)            
            let sub dispatch =
                runner.Msgs.Add (handleRunnerMsgs client dispatch)
                runner.Start ()
            Running {
                workflow = state.selectedWorkflow
                runner = runner
                state = "Started"
                paused = false
                lastDataValue = 0
                msgs = []
            }, Cmd.ofSub sub
        | _ -> NotRunning state, Cmd.none
    | Quit ->        
        Console.Clear ()
        Environment.Exit 0
        NotRunning state, Cmd.none
    | _ -> NotRunning state, Cmd.none
    
let updateRunning state msg =
    match msg with
    | ControlMsg m ->
        match m with
        | Control.DataUpdate value ->
            state.runner.DataUpdate value
            Running {state with lastDataValue = value}
        | Control.InputReceived input ->
            state.runner.InputReceived input
            Running state
        | _ -> Running state
    | WorkflowMsg m ->
        match m with
        | Stop ->
            state.runner.Cancel ()
            Done {
               state with
                state = sprintf "%s (CANCELLED)" state.state
            }
        | Finish ->
            Done state
        | Pause paused ->
            if paused then
                state.runner.Pause ()
            else
                state.runner.Resume ()
            Running {state with paused = paused}
        | _ -> Running state
    | SetState value -> Running {state with state = value}
    | AddMsg msg -> Running {state with msgs = msg :: state.msgs}
    | ClearMsgs -> Running {state with msgs = []}
    | _ -> Running state

let updateDone state msg =
    match msg with
    | WorkflowMsg m ->
        match m with
        | Reset -> NotRunning {selectedWorkflow = state.workflow}
        | _ -> Done state
    | _ -> Done state

let update msg model =
    match msg with
    | ControlMsg m ->
        match m with
        | Control.ServerExit ->
            Console.Clear ()
            printfn "Server Exited"
            Environment.Exit 0
        | _ -> ()
    | _ -> ()
    match model.state with
    | NotRunning state  ->
        let newRunState, cmd = updateNotRunning model.client state msg 
        let newState = {model with state = newRunState}
        newState, cmd
    | Running state  -> {model with state = updateRunning state msg}, Cmd.none
    | Done state -> {model with state = updateDone state msg}, Cmd.none

let viewNotRunning state dispatch : List<View>=
    [
        yield frameView [
            Text "Workflow"
            Styles [
                Pos (AbsPos 0, AbsPos 0)
                Dim (Fill, Dimension.FillMargin 1)
            ]
        ][
            yield listView [
                Styles [
                    Pos (AbsPos 0, AbsPos 0)
                    Dim (Fill, FillMargin 1)
                ]
                Value state.selectedWorkflow
                Items workflows
                OnChanged (fun wf -> dispatch (WorkflowSelected wf))
            ]    
            yield button [
                Text "Start"
                Styles [Pos (AbsPos 0, PercentPos 99.0)]
                OnClicked  (fun _ -> dispatch (WorkflowMsg Start))
            ]
        ]
        
        yield button [
            Styles [Pos (AbsPos 0, PercentPos 99.0)]
            Text "Quit"
            OnClicked  (fun _ -> dispatch Quit)
        ]        
    ]

let viewRunning state dispatch : List<View> =
    [
        let wfRow = 0
        let wfText = sprintf "Workflow: %s " (workflowToText state.workflow)
        yield label [
            Text wfText
            Styles [Pos (AbsPos 0, AbsPos wfRow)]
        ]
        let pauseText = if state.paused then "Resume" else "Pause"
        yield button [
            Text pauseText
            Styles [Pos (AbsPos (wfText.Length + 5), AbsPos wfRow)]
            OnClicked (fun _ -> dispatch (not state.paused |> Pause |> WorkflowMsg))
        ]
        yield button [
            Text "Stop"
            Styles [Pos (AbsPos (wfText.Length + pauseText.Length + 10), AbsPos wfRow)]
            OnClicked (fun _ -> dispatch (WorkflowMsg Stop))
        ]
        
        let stateRow = wfRow + 1
        yield label [
            Text (sprintf "State: %s%s" state.state (if state.paused then " (PAUSED)" else ""))
            Styles [Pos (AbsPos 0, AbsPos stateRow)]
        ]
        
        let dataRow = stateRow + 1
        yield label [
            Text (sprintf "Data: %d" state.lastDataValue)
            Styles [Pos (AbsPos 0, AbsPos dataRow)]
        ]
        
        let msgsRow = dataRow + 1
        yield frameView [
            Text "Messages"
            Styles [
                Pos (AbsPos 0, AbsPos msgsRow)
                Dim (Fill, Dimension.FillMargin 1)
            ]
        ][
            yield listView [
                Styles [
                    Pos (AbsPos 0, AbsPos 0)
                    Dim (Fill, FillMargin 1)
                ]
                Items (state.msgs |> List.indexed)
            ]    
        ]        
    ]

let viewDone state dispatch : List<View> =
    [
        let wfRow = 0
        let wfText = sprintf "Workflow: %s (DONE)" (workflowToText state.workflow)
        yield label [
            Text wfText
            Styles [Pos (AbsPos 0, AbsPos wfRow)]
        ]
        yield button [
            Text "Reset"
            Styles [Pos (AbsPos (wfText.Length + 5), AbsPos wfRow)]
            OnClicked (fun _ -> dispatch (WorkflowMsg Reset))
        ]
        
        let stateRow = wfRow + 1
        yield label [
            Text (sprintf "Final State: %s" state.state)
            Styles [Pos (AbsPos 0, AbsPos stateRow)]
        ]
        
        let dataRow = stateRow + 1
        yield label [
            Text (sprintf "Data: %d" state.lastDataValue)
            Styles [Pos (AbsPos 0, AbsPos dataRow)]
        ]
        
        let msgsRow = dataRow + 1
        yield frameView [
            Text "Messages"
            Styles [
                Pos (AbsPos 0, AbsPos msgsRow)
                Dim (Fill, Dimension.FillMargin 1)
            ]
        ][
            yield listView [
                Styles [
                    Pos (AbsPos 0, AbsPos 0)
                    Dim (Fill, FillMargin 1)
                ]
                Items (state.msgs |> List.indexed)
            ]    
        ]        
    ]
    
let view model dispatch =    
    page [
        window [Title "Control"][
            match model.state with
            | NotRunning state  -> yield! viewNotRunning state dispatch
            | Running state  -> yield! viewRunning state dispatch
            | Done state -> yield! viewDone state dispatch
        ]
    ]
    
[<EntryPoint>]
let main _ =
    printfn "Waiting for device to start up..."
    let client = Client (Common.pipeName)
    
    Program.mkProgram init update view
    |> Program.withSubscription (fun model ->
            let sub dispatch =
                model.client.MsgRecvd.Add (fun msg ->
                    Application.MainLoop.Invoke (Action (fun _ -> dispatch (ControlMsg msg)))                                       
                )
            Cmd.ofSub sub
        )
    |> Program.runWith client
        
    0 // return an integer exit code
