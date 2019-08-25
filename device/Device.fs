module WorkflowDemo.Device.Main

open System
open Terminal.Gui
open Terminal.Gui.Elmish

[<EntryPoint>]
let main _ =
    
    let clientMgr = ClientMgr.ClientMgr ()

    Program.mkSimple Model.init Update.update View.view
    |> Program.withSubscription (fun _ ->            
            let sub dispatch =
                // If messages go into the main loop too fast they get lost so
                // introduce a delay here
                let agent = MailboxProcessor<Model.Message>.Start (fun inbox ->
                    let rec loop (stopwatch: System.Diagnostics.Stopwatch) =
                        async {                            
                            let! msg = inbox.Receive ()
                            let remain = int (25.0 - stopwatch.Elapsed.TotalMilliseconds)
                            if remain > 0 then
                                do! Async.Sleep remain
                            Application.MainLoop.Invoke (Action (fun _ -> dispatch msg))
                            stopwatch.Restart ()
                            return! loop stopwatch
                        }
                    loop (System.Diagnostics.Stopwatch.StartNew ())
                )
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
