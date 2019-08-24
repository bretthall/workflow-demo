module WorkflowDemo.Device.Update

open System
open Terminal.Gui.Elmish
open WorkflowDemo.Common

let update (msg: Model.Message) (model: Model.Model) =
  match msg with
  | Model.DeviceMsg dm -> model, Cmd.none
  | Model.DataMsg dm ->
    match dm with
    | Model.IncData -> {model with dataValue = model.dataValue + 1}, Cmd.none
    | Model.ResetData -> {model with dataValue = 0}, Cmd.none
  | Model.InputMsg m ->
    match m with
    | Model.StartInput prompt ->
      {model with inputState = Some {prompt = prompt; current = ""}}, Cmd.none
    | Model.InputUpdate value -> 
      {model with inputState = model.inputState |> Option.map (fun s -> {s with current = value})}, Cmd.none
    | Model.InputDone ->
      //TODO: Send input
      {model with inputState = None}, Cmd.none
  | Model.Quit ->
    model.clientMgr.Stop ()
    Console.Clear ()
    Environment.Exit 0
    model, Cmd.none
    