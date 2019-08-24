module WorkflowDemo.Device.Main

open System
open Terminal.Gui
open Terminal.Gui.Elmish

[<EntryPoint>]
let main _ =
    
    let clientMgr = ClientMgr.ClientMgr ()

    Program.mkSimple Model.init Update.update View.view
    |> Program.withSubscription (fun _ ->
            let subClient dispatch =
                clientMgr.MsgRecvd.Add (fun msg -> 
                    Application.MainLoop.Invoke (Action (fun _ -> dispatch (Model.DeviceMsg msg)))
                )
            let subData dispatch =
                let rec updateData () =
                    async {
                        do! Async.Sleep 1000
                        Application.MainLoop.Invoke (Action (fun _ -> dispatch (Model.DataMsg Model.IncData)))
                        return! updateData ()
                    }
                Async.Start (updateData ())
            Cmd.batch [Cmd.ofSub subClient; Cmd.ofSub subData]
        )
    |> Program.runWith clientMgr

    0 // return an integer exit code
