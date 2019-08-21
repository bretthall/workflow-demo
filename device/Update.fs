module WorkflowDemo.Device.Update

open System
open Terminal.Gui.Elmish

let update (msg: Model.Message) (model: Model.Model) =
    match msg with
    | Model.IncFoo -> {model with foo = model.foo + 1}, Cmd.none
    | Model.IncBar -> {model with bar = model.bar + 1}, Cmd.none
    | Model.Quit  ->
        Console.Clear ()
        Environment.Exit 0
        model, Cmd.none
    