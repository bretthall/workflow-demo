module WorkflowDemo.Control.PipeClient

open System
open System.Threading.Tasks.Dataflow
open MBrace.FsPickler

type ClientMsg<'msg> = 
    | Send of 'msg
    | Stop 

type Client<'outMsg, 'inMsg>(pipeName: string) = 
    
    let msgRecvd = Event<'inMsg> ()

    let pipe = new IO.Pipes.NamedPipeClientStream (".", pipeName, IO.Pipes.PipeDirection.InOut)
    do pipe.Connect ()
    let queue = BufferBlock<ClientMsg<'outMsg>> ()  
    let pickler = FsPickler.CreateBinarySerializer ()
    do
        async {
            try 
                pickler.SerializeSequence (
                    pipe, 
                    () |> Seq.unfold (fun _ -> 
                        let msg = queue.Receive ()
                        match msg with
                        | Stop -> None
                        | Send pipeMsg -> Some (pipeMsg, ())
                    )
                ) |> ignore
            with
            | _ -> 
                pipe.Close ()
        } |> Async.Start
    do 
        async {
            try
                for msg in pickler.DeserializeSequence<'inMsg>(pipe) do
                    msgRecvd.Trigger(msg)
            with
            | _ -> () //closing pipe handled in sending async above
        } |> Async.Start
        

    member __.Send msg = queue.Post (Send msg) |> ignore
    member __.Stop () = queue.Post Stop |> ignore
    member __.MsgRecvd = msgRecvd.Publish

