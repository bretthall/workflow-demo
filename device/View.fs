module WorkflowDemo.Device.View

open Terminal.Gui.Elmish

let view (model: Model.Model) dispatch =
    let labelText = sprintf "foo = %d   bar = %d" model.foo model.bar
    
    page [
        window [Title "Hello"][
            label [Text labelText; Styles [Pos (AbsPos 0, AbsPos 0)]]
            button [Styles [Pos (AbsPos 0, AbsPos 1)]; Text "IncFoo"; OnClicked  (fun _ -> dispatch Model.IncFoo)]
            button [Styles [Pos (AbsPos 0, AbsPos 2)]; Text "Quit"; OnClicked  (fun _ -> dispatch Model.Quit)]
        ]
    ]