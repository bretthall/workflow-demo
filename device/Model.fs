module WorkflowDemo.Device.Model

open Terminal.Gui.Elmish
open WorkflowDemo.Common

type Model = {
    clientMgr: ClientMgr.ClientMgr
    foo: int
    bar: int    
}

let init clientMgr =
    let model = {
        clientMgr = clientMgr
        foo = 0
        bar = 0
    }
    model, Cmd.none

type Message =
    | DeviceMsg of Device.Msg
    | IncFoo
    | Quit