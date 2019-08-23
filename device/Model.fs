module WorkflowDemo.Device.Model

open Terminal.Gui.Elmish
open WorkflowDemo.Common

type State = 
    | Good
    | Bad
    | Ugly
  
let nextState  = function
    | Good -> Bad
    | Bad -> Ugly
    | Ugly -> Good

type InputState = {
    prompt: string
    current: string
}
                
type Model = {
    clientMgr: ClientMgr.ClientMgr
    state: State
    inputState: Option<InputState>
    msgs: List<string>
}

let init clientMgr =
    let model = {
        clientMgr = clientMgr
        state = Good
        inputState = Some {prompt = "prompt"; current = "current"}
        msgs = ["one"; "two"; "three"]
    }
    model, Cmd.none

type InputMsg = 
    | StartInput of string
    | InputUpdate of string
    | InputDone
      
type Message =
    | DeviceMsg of Device.Msg
    | InputMsg of InputMsg
    | Quit