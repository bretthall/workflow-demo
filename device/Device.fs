module WorkflowDemo.Device.Main

open System
open Terminal.Gui
open Terminal.Gui.Elmish
open WorkflowDemo.Common.DelayQueue

[<EntryPoint>]
let main _ =
    
    let clientMgr = ClientMgr.ClientMgr ()

    Program.mkSimple Model.init Update.update View.view
    |> Program.withSubscription (fun _ ->            
            let sub dispatch =
                // If messages go into the main loop too fast they get lost so
                // introduce a delay here
                let agent = delayQueue 25.0<ms>  (fun msg -> Application.MainLoop.Invoke (Action (fun _ -> dispatch msg)))
                clientMgr.MsgRecvd.Add (fun msg -> 
                    agent.Post (Model.DeviceMsg msg)
                )
                let rec updateData () =
                    async {
                        do! Async.Sleep 1000
                        agent.Post (Model.DataMsg Model.IncData)
                        return! updateData ()
                    }
                Async.Start (updateData ())
            Cmd.ofSub sub
        )
    |> Program.runWith clientMgr

    0 // return an integer exit code
