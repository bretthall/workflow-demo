module WorkflowDemo.Control.Main


open System
open Terminal.Gui
open Terminal.Gui.Elmish

open WorkflowDemo.Common

type Client = PipeClient.Client<Device.Msg, Control.Msg>

type Model = {
    client: Client
    foo: int
    bar: int
}

let init client = {
    client = client
    foo = 0
    bar = 0
}

type Msg =
    | ControlMsg of Control.Msg
    | IncBar
    | Quit
    
let update msg model =
    match msg with
    | ControlMsg cm ->
        match cm with
        | Control.Msg.Foo newFoo -> {model with foo = newFoo}
        | Control.Msg.ServerExit ->
            Console.Clear ()
            Environment.Exit 0
            model
    | IncBar ->
        let newBar = model.bar + 1
        model.client.Send (Device.Bar newBar)
        {model with bar = newBar}
    | Quit ->
        Console.Clear ()
        Environment.Exit 0
        model        
        
let view model dispatch =
    let labelText = sprintf "foo = %d   bar = %d" model.foo model.bar
    
    page [
        window [Title "Control"][
            label [Text labelText; Styles [Pos (AbsPos 0, AbsPos 0)]]
            button [Styles [Pos (AbsPos 0, AbsPos 1)]; Text "IncBar"; OnClicked  (fun _ -> dispatch IncBar)]
            button [Styles [Pos (AbsPos 0, AbsPos 2)]; Text "Quit"; OnClicked  (fun _ -> dispatch Quit)]
        ]
    ]
    
[<EntryPoint>]
let main _ =
    printfn "Waiting for device to start up..."
    let client = Client (Common.pipeName)
    
    Program.mkSimple init update view
    |> Program.withSubscription (fun model ->
            let sub dispatch =
                model.client.MsgRecvd.Add (fun msg ->
                    Application.MainLoop.Invoke (Action (fun _ -> dispatch (ControlMsg msg)))                                       
                )
            Cmd.ofSub sub
        )
    |> Program.runWith client
        
    0 // return an integer exit code
