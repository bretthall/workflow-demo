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

type Model = {
  selectedWorkflow: WorkflowName
  running: bool
}

type Msg =
| SelectWorkflow of WorkflowName
| Start
| Stop

let workflows = ["one"; "two"; "three"]
let init() : Model = 
  {
    selectedWorkflow = List.head workflows
    running = false
  }
  // match BrowserLocalStorage.load Thoth.Json.Decode.int "value" with
  // | Ok value -> value
  // | Error _ -> 0

// UPDATE
let update (msg:Msg) (model:Model) =
  let newModel =   
    match msg with
    | SelectWorkflow wf -> {model with selectedWorkflow = wf}
    | Start -> {model with running = true}    
    | Stop -> {model with running = false}    
  //BrowserLocalStorage.save "value" newModel
  newModel

let selectWorkflowView (model: Model) dispatch = 
  div []
    [ 
      select [] (seq {for wf in workflows do yield option [] [str wf]})
      button [OnClick (fun _ -> dispatch Start)] [str "Start"]
    ]

let runningWorkflowView (model:Model) dispatch = 
  div []
    [
      str (sprintf "Running %s" model.selectedWorkflow)
      button [OnClick (fun _ -> dispatch Stop)] [str "Stop"]
    ]  
    
// VIEW (rendered with React)
let view (model:Model) dispatch =
  if model.running then
    runningWorkflowView model dispatch
  else
    selectWorkflowView model dispatch

// App
Program.mkSimple init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
