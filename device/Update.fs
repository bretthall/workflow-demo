module WorkflowDemo.Device.Update

open System
open Terminal.Gui.Elmish
open WorkflowDemo.Common

let update (msg: Model.Message) (model: Model.Model) =
  match msg with
  | Model.DeviceMsg dm -> model, Cmd.none
  | Model.InputMsg _ -> model, Cmd.none        
  | Model.Quit ->
    model.clientMgr.Stop ()
    Console.Clear ()
    Environment.Exit 0
    model, Cmd.none
    