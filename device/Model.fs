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
    dataValue: int
    inputState: Option<InputState>
    msgs: List<string>
}

let init clientMgr =
    let model = {
        clientMgr = clientMgr
        dataValue = 0
        state = Good
        inputState = Some {prompt = "prompt"; current = "current"}
        msgs = ["one"; "two"; "three"]
    }
    model, Cmd.none

type DataMsg =
    | IncData
    | ResetData
    
type InputMsg = 
    | StartInput of string
    | InputUpdate of string
    | InputDone
      
type Message =
    | DeviceMsg of Device.Msg
    | DataMsg of DataMsg 
    | InputMsg of InputMsg
    | Quit