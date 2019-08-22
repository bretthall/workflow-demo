module WorkflowDemo.Device.Main

open System
open Terminal.Gui
open Terminal.Gui.Elmish

[<EntryPoint>]
let main argv =
    
    let clientMgr = ClientMgr.ClientMgr ()

    Program.mkProgram Model.init Update.update View.view
    |> Program.withSubscription (fun _ ->
            let sub dispatch =
                clientMgr.MsgRecvd.Add (fun msg -> 
                    Application.MainLoop.Invoke (Action (fun _ -> dispatch (Model.DeviceMsg msg)))
                )
            Cmd.ofSub sub
        )
    |> Program.runWith clientMgr
    
    clientMgr.Stop ()

    0 // return an integer exit code
