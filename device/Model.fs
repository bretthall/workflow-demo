module WorkflowDemo.Device.Model

open Terminal.Gui.Elmish

type Model = {
    foo: int
    bar: int    
}

let init _ =
    let model = {
        foo = 0
        bar = 0
    }
    model, Cmd.none

type Message =
    | IncFoo
    | IncBar
    | Quit