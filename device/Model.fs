module WorkflowDemo.Device.Model

open WorkflowDemo.Common
open WorkflowDemo.Common.Device

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
        inputState = None
        msgs = []
    }
    model

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