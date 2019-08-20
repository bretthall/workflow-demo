module WorkflowDemo.Device.Main

open System

open WorkflowDemo.Common

type ClientMgrMsg = 
    | NewClient of PipeServer.ClientDispatch<Control.Msg> * AsyncReplyChannel<unit>
    | SendMsg of Control.Msg

type ClientMgr () = 
    
    let hub = PipeServer.ClientHub<Control.Msg, Device.Msg>(Common.pipeName)
    
    let agent = MailboxProcessor<ClientMgrMsg>.Start (fun inbox ->
        inbox.Error.Add (fun exc -> printfn "Got exception in client mgr agent: %A" exc)

        let rec loop curFoo = 
            async {
                match! inbox.Receive() with
                | ClientMgrMsg.NewClient (client, reply) -> 
                    client (Control.Foo curFoo)
                    reply.Reply ()
                    return! loop curFoo
                | SendMsg msg ->
                    let newFoo = 
                        match msg with 
                        | Control.Foo newFoo -> newFoo
                        | _ -> curFoo
                    hub.Broadcast msg
                    return! loop newFoo                            
            }
        loop 0
    )

    do hub.Start (fun client -> agent.PostAndReply (fun r -> ClientMgrMsg.NewClient (client, r)))
    
    member __.Broadcast msg = agent.Post (SendMsg msg)
    member __.Stop () = hub.Stop ()
    member __.MsgRecvd = hub.MsgRecvd

[<EntryPoint>]
let main argv =
    
    let clientMgr = ClientMgr ()

    let view = View.View ()
    let rec loop num = 
        async {
            do! Async.Sleep 1000
            view.SetBar num
            return! loop (num + 1)
        }
    loop 1 |> Async.Start
    view.Run ()
    
    clientMgr.Stop ()

    0 // return an integer exit code
