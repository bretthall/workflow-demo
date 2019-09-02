module WorkflowDemo.Control.PipeClient

open System
open System.Threading.Tasks.Dataflow
open MBrace.FsPickler

type ClientMsg<'msg> = 
    | Send of 'msg
    | Stop
    
let private log = NLog.LogManager.GetLogger "PipeClient"

type Client<'outMsg, 'inMsg>(pipeName: string) = 
    
    let msgRecvd = Event<'inMsg> ()

    do log.Info (sprintf "Connecting to pipe '%s'" pipeName)
    
    let pipe = new IO.Pipes.NamedPipeClientStream (".", pipeName, IO.Pipes.PipeDirection.InOut)
    do
        pipe.Connect ()
        log.Info "Connected to pipe"
    let queue = BufferBlock<ClientMsg<'outMsg>> ()  
    let pickler = FsPickler.CreateBinarySerializer ()
    do
        async {
            log.Info "Starting serializer"
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
            | exc ->
                log.Info (sprintf "Closing pipe: %A" exc)
                pipe.Close ()
        } |> Async.Start
    do 
        async {
            log.Info "Starting deserializer"
            try
                for msg in pickler.DeserializeSequence<'inMsg>(pipe) do
                    msgRecvd.Trigger(msg)
            with
            | exc ->
                log.Info (sprintf "Deserializer stopping: %A" exc)
                () //closing pipe handled in sending async above
        } |> Async.Start
        

    member __.Send msg = queue.Post (Send msg) |> ignore
    member __.Stop () = queue.Post Stop |> ignore
    member __.MsgRecvd = msgRecvd.Publish

