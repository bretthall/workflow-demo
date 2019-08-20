module WorkflowDemo.Device.View

open System
open Terminal.Gui
open NStack

let ustr (x: string) = ustring.Make x

type UpdateMsg =
    | Foo of int
    | Bar of int
    
type View () = 

    let updated = Event<UpdateMsg> ()
    
    let mutable foo = 0
    let mutable bar = 0

    let buildLabelText () = sprintf "foo = %d   bar = %d" foo bar |> ustr

    do Application.Init ()
    let top = Application.Top
    let win = Window (ustr "Hello", X=Pos.op_Implicit(0), Y=Pos.op_Implicit(1), Width=Dim.Fill(), Height=Dim.Fill())
    let label = Label (0, 0, buildLabelText ())
    let updateLabelText () = label.Text <- buildLabelText ()
    do win.Add (label)
    let incFooButton = Button (0, 1, ustr "Inc Foo", Clicked  = Action (fun _ -> foo <- foo + 1; updateLabelText ()))
    do win.Add (incFooButton)    
    let quitButton = Button (0, 2, ustr "Quit", Clicked = System.Action (fun _ -> Application.Top.Running <- false))
    do win.Add (quitButton)
    do top.Add (win)

    member __.Run () = Application.Run ()

    member __.SetFoo value = Application.MainLoop.Invoke (fun _ ->
        foo <- value
        label.Text <- buildLabelText ()
        updated.Trigger (Foo value)
    )
    
    member __.SetBar value = Application.MainLoop.Invoke (fun _ ->
        bar <- value
        label.Text <- buildLabelText ()
        updated.Trigger (Bar value)
    )
    
    member __.Updated = updated.Publish
    