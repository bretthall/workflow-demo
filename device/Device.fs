module WorkflowDemo.Device.Main

open Terminal.Gui.Elmish

[<EntryPoint>]
let main _ =
    
    let clientMgr = ClientMgr.ClientMgr ()

    Program.mkSimple Model.init Update.update View.view
    |> Program.withSubscription (fun _ ->            
            let sub dispatch =
                clientMgr.MsgRecvd.Add (fun msg ->
                    dispatch (Model.DeviceMsg msg)
                )
                let rec updateData () =
                    async {
                        do! Async.Sleep 1000
                        dispatch (Model.DataMsg Model.IncData)
                        return! updateData ()
                    }
                Async.Start (updateData ())
            Cmd.ofSub sub
        )
    |> Program.runWith clientMgr

    0 // return an integer exit code
