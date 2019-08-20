module WorkflowDemo.Device.View

open Terminal.Gui
open NStack

let ustr (x: string) = ustring.Make x

type View () = 

    do Application.Init ()
    let top = Application.Top
    let win = Window (ustr "Hello", X=Pos.op_Implicit(0), Y=Pos.op_Implicit(1), Width=Dim.Fill(), Height=Dim.Fill())
    let label = Label (0, 0, ustr "Foo = 0")
    do win.Add (label)
    let quitButton = Button (0, 1, ustr "Quit", Clicked = System.Action (fun _ -> Application.Top.Running <- false))
    do win.Add (quitButton)
    do top.Add (win)

    member __.Run () = Application.Run ()

    member __.SetFoo value = Application.MainLoop.Invoke (fun _ -> (label.Text <- (sprintf "foo = %d" value |> ustr)))
