module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
// MODEL

type WorkflowName = string

type State = 
  | Good
  | Bad
  | Ugly

let nextState  = function
  | Good -> Bad
  | Bad -> Ugly
  | Ugly -> Good

let stateToText = function
  | Good -> "Good"
  | Bad -> "Bad"
  | Ugly -> "Ugly"

let stateToColor = function
  | Good -> "green"
  | Bad -> "red"
  | Ugly -> "brown"

type InputState = {
  prompt: string
  current: string
}

type TimerState = {
  start: System.DateTime
  duration: System.TimeSpan
}

type RunState = {
  paused: bool
  state: State
  messages: List<string>
  nextMessage: int
  input: Option<InputState>
}

type Model = {
  selectedWorkflow: WorkflowName
  runState: Option<RunState>
}

type InputMsg = 
  | StartInput of string
  | InputUpdate of string
  | InputDone

type RunMsg = 
  | Pause
  | SetState of State
  | AddMsg
  | NextState
  | Input of InputMsg
  
type Msg =
| SelectWorkflow of WorkflowName
| Start
| Stop
| Run of RunMsg

let workflows = ["one"; "two"; "three"]

let initRunState  = {
  paused = false
  state = Good
  messages = []
  nextMessage = 0
  input = None
}

let init() = 
  {
    selectedWorkflow = List.head workflows
    runState = None
  }, Cmd.none
  // match BrowserLocalStorage.load Thoth.Json.Decode.int "value" with
  // | Ok value -> value
  // | Error _ -> 0

let updateInput msg model = 
  match msg with
  | StartInput prompt -> {model with input = Some {prompt = prompt; current = ""}}
  | InputUpdate input -> {model with input = model.input |> Option.map (fun m -> {m with current = input})}
  | InputDone -> 
    match model.input with
    | Some input -> {model with input = None; messages = (sprintf "Input: %s" input.current) :: model.messages}
    | None -> model

let updateRunState msg model =
  match msg with
  | Pause -> {model with paused = not model.paused}, Cmd.none
  | SetState state -> {model with state = state}, Cmd.none
  | AddMsg -> ({
    model with 
      messages = (sprintf "Message number %d" model.nextMessage)::model.messages
      nextMessage = model.nextMessage + 1
    }, Cmd.none)
  | NextState -> {model with state = nextState model.state}, Cmd.none
  | Input imsg -> updateInput imsg model, Cmd.none

// UPDATE
let update (msg:Msg) (model:Model) =
  let newModel, cmd =   
    match msg with
    | SelectWorkflow wf -> {model with selectedWorkflow = wf}, Cmd.none
    | Start -> {model with runState = Some initRunState}, Cmd.none    
    | Stop -> {model with runState = None}, Cmd.none
    | Run rmsg -> 
      match model.runState with
      | Some runState -> 
        let newRunState, cmd = updateRunState rmsg runState  
        {model with runState = Some newRunState}, cmd
      | None -> model, Cmd.none            
  //BrowserLocalStorage.save "value" newModel
  newModel, cmd

let selectWorkflowView (model: Model) dispatch = 
  div []
    [ 
      select [
        DefaultValue model.selectedWorkflow
        OnChange (fun e -> dispatch (SelectWorkflow e.Value))
      ] (seq {
        for wf in workflows do 
          yield option [Value wf] [str wf]
      })
      button [OnClick (fun _ -> dispatch Start)] [str "Start"]
    ]

let runningWorkflowView workflow (model:RunState) dispatch = 
  div []
    [
      yield str (sprintf "Running %s" workflow)
      yield button [OnClick (fun _ -> dispatch Stop)] [str "Stop"]
      yield button [OnClick (fun _ -> dispatch (Run Pause))] [str (if model.paused then "Resume" else "Pause")]
      let paused = model.paused
      yield div [
        Style [Color (stateToColor model.state)]
      ] [
        str (stateToText model.state)
        button [OnClick (fun _ -> dispatch (Run NextState)); Disabled paused] [str "Next"]
      ]

      match model.input with
      | Some inputState -> yield div [] [
        str inputState.prompt
        input [OnChange (fun e -> dispatch (InputUpdate e.Value |> Input |> Run)); Disabled paused]
        button [OnClick (fun e -> dispatch (Input InputDone |> Run)); Disabled paused] [str "Ok"]
        ]
      | None -> yield div [] [
        button [
          (OnClick (fun _ -> dispatch (StartInput "Input:" |> Input |> Run)))
          Disabled paused
        ] [str "Get Input"]]      

      yield h2 [] [
        str "Messages:"
        button [OnClick (fun _ -> dispatch (Run AddMsg)); Disabled paused] [str "Add"]
      ]
      for m in model.messages do
        yield div [] [str m]
    ]  
    
// VIEW (rendered with React)
let view (model:Model) dispatch =
  match model.runState with
  | Some runState -> 
    runningWorkflowView model.selectedWorkflow runState dispatch
  | None ->
    selectWorkflowView model dispatch

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
