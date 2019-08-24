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
    
let workflows =
    [
        Workflows.Reset
    ] |> List.map (fun wf -> (wf, workflowToText wf))

type NotRunningState = {
    selectedWorkflow: Workflows.Workflow
}

type RunningState = {
    workflow: Workflows.Workflow
    state: string
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

let init client = {
    client = client
    state = NotRunning {selectedWorkflow = Workflows.Reset}
}

type WorkflowMsg =
    | Start
    | Pause
    | Stop
    | Finish
    | Reset
    
type Msg =
    | ControlMsg of Control.Msg
    | WorkflowSelected of Workflows.Workflow
    | WorkflowMsg of WorkflowMsg
    | SetState of string
    | AddMsg of string
    | Quit

let updateNotRunning (state: NotRunningState) msg =
    match msg with
    | ControlMsg _ -> NotRunning state
    | WorkflowSelected wf -> NotRunning {state with selectedWorkflow = wf}
    | WorkflowMsg m ->
        match m with
        | Start ->
            Running {
                workflow = state.selectedWorkflow
                state = "Started"
                lastDataValue = 0
                msgs = []                
            }
        | Pause | Stop | Finish | Reset -> NotRunning state
    | SetState _ | AddMsg _ -> NotRunning state 
    | Quit ->        
        Console.Clear ()
        Environment.Exit 0
        NotRunning state
    
let updateRunning state msg =
    match msg with
    | ControlMsg m ->
        match m with
        | Control.DataUpdate value -> Running {state with lastDataValue = value}
        | _ -> Running state
    | WorkflowSelected _ -> Running state
    | WorkflowMsg m ->
        match m with
        | Stop ->
            Done {
               state with
                state = sprintf "%s (CANCELLED)" state.state
            }
        | Finish ->
            Done state
        | Pause -> Running state //TODO: Handle pause
        | Start | Reset -> Running state
    | SetState value -> Running {state with state = value}
    | AddMsg msg -> Running {state with msgs = msg :: state.msgs} 
    | Quit -> Running state

let updateDone state msg =
    match msg with
    | ControlMsg _ -> Done state
    | WorkflowSelected _ -> Done state
    | WorkflowMsg m ->
        match m with
        | Reset -> NotRunning {selectedWorkflow = state.workflow}
        | Start | Pause | Stop | Finish -> Done state
    | SetState _ | AddMsg _ -> Done state
    | Quit -> Done state

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
    | NotRunning state  -> {model with state = updateNotRunning state msg}
    | Running state  -> {model with state = updateRunning state msg}
    | Done state -> {model with state = updateDone state msg}

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
        let pauseText = "Pause"
        yield button [
            Text pauseText
            Styles [Pos (AbsPos (wfText.Length + 5), AbsPos wfRow)]
            OnClicked (fun _ -> dispatch (WorkflowMsg Pause))
        ]
        yield button [
            Text "Stop"
            Styles [Pos (AbsPos (wfText.Length + pauseText.Length + 10), AbsPos wfRow)]
            OnClicked (fun _ -> dispatch (WorkflowMsg Stop))
        ]
        
        let stateRow = wfRow + 1
        yield label [
            Text (sprintf "State: %s" state.state)
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
    
    Program.mkSimple init update view
    |> Program.withSubscription (fun model ->
            let sub dispatch =
                model.client.MsgRecvd.Add (fun msg ->
                    Application.MainLoop.Invoke (Action (fun _ -> dispatch (ControlMsg msg)))                                       
                )
            Cmd.ofSub sub
        )
    |> Program.runWith client
        
    0 // return an integer exit code
