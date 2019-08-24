module WorkflowDemo.Device.Update

open System

open WorkflowDemo.Common
open WorkflowDemo.Device.Model

let update (msg: Model.Message) (model: Model.Model) =
  match msg with
  | Model.DeviceMsg dm ->
    match dm with
    | Device.SetState state -> {model with state = state}
    | Device.RequestInput prompt -> {model with inputState = Some {prompt = prompt; current = ""}}
    | Device.CancelInput -> {model with inputState = None}
    | Device.AddMsg msg -> {model with msgs = msg :: model.msgs}
  | Model.DataMsg dm ->
    match dm with
    | Model.IncData ->
      let newData = model.dataValue + 2
      model.clientMgr.Broadcast (Control.DataUpdate newData)
      {model with dataValue = newData}
    | Model.ResetData -> {model with dataValue = 0}
  | Model.InputMsg m ->
    match m with
    | Model.StartInput prompt ->
      {model with inputState = Some {prompt = prompt; current = ""}}
    | Model.InputUpdate value -> 
      {model with inputState = model.inputState |> Option.map (fun s -> {s with current = value})}
    | Model.InputDone ->
      model.inputState |> Option.iter (fun s ->
        model.clientMgr.Broadcast (Control.InputReceived s.current)
      )
      {model with inputState = None}
  | Model.Quit ->
    model.clientMgr.Stop ()
    Console.Clear ()
    Environment.Exit 0
    model
    