module WorkflowDemo.Device.Update

open System

open WorkflowDemo.Common
open WorkflowDemo.Device.Model

let private log = NLog.LogManager.GetLogger "update"

let update (msg: Model.Message) (model: Model.Model) =
    match msg with
    | Model.DeviceMsg dm ->
        log.Info (sprintf "Got device msg: %A" dm)
        match dm with
        | Device.SetState state -> {model with state = state; msgs = "got set state" :: model.msgs}
        | Device.RequestInput prompt -> {model with inputState = Some {prompt = prompt; current = ""}; msgs = "got start input" :: model.msgs}
        | Device.CancelInput -> {model with inputState = None; msgs = "got cancel input" :: model.msgs}
        | Device.AddMsg msg -> {model with msgs = msg :: model.msgs}
        | Device.ClearMsgs -> {model with msgs = []}
        | Device.ResetData -> {model with dataValue = 0}
    | Model.DataMsg dm ->
        match dm with
        | Model.IncData ->
            let newData = model.dataValue + 1
            model.clientMgr.Broadcast (Control.DataUpdate newData)
            {model with dataValue = newData}
        | Model.ResetData ->
            log.Info "Resetting data"
            {model with dataValue = 0}
    | Model.InputMsg m ->
        log.Info (sprintf "Got input msg: %A" m)
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
        log.Info "Got quit msg"
        model.clientMgr.Stop ()
        Console.Clear ()
        Environment.Exit 0
        model
        