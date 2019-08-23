module WorkflowDemo.Device.View

open Terminal.Gui
open Terminal.Gui.Elmish


type WorkflowName = string

module private Private = 

    let stateToText = function
      | Model.Good -> "Good"
      | Model.Bad -> "Bad"
      | Model.Ugly -> "Ugly"

    let stateToColor = function
      | Model.Good -> Color.Green
      | Model.Bad -> Color.Red
      | Model.Ugly -> Color.Brown

open Private

let view (model: Model.Model) dispatch =

    page [
      window [
          Title "Device"
          Styles [Colors (Color.BrightGreen, Color.Black)]
          
      ][
          yield label [
              Styles [
                Colors (stateToColor model.state, Color.Black)
                Pos (AbsPos 0, AbsPos 0)
              ]
              Text (stateToText model.state)                
          ]
          
          match model.inputState with
          | Some inputState -> 
            let inputLabel = label [
              Styles [Pos (AbsPos 0, AbsPos 1)]
              Text inputState.prompt
            ]
            yield inputLabel
            let inputField = textField [
              Value inputState.current
              OnChanged (fun v -> dispatch (Model.InputUpdate v |> Model.InputMsg))
              Styles [
                Pos (AbsPos inputState.prompt.Length, AbsPos 1)
                Dim (AbsDim 30, AbsDim 1)
              ]
            ]
            yield inputField
            yield button [
              Text "Send"
              OnClicked (fun _ -> dispatch (Model.InputDone |> Model.InputMsg))
              Styles [
                Pos (AbsPos (inputState.prompt.Length + 30), AbsPos 1)
              ]
            ]
          | None -> yield! []

          yield frameView [
            Text "Messages"
            Styles [
              Pos (AbsPos 0, AbsPos 3)
              Dim (Fill, Dimension.FillMargin 1)
            ]
          ][
            yield listView [
              Styles [
                Pos (AbsPos 0, AbsPos 0)
                Dim (Fill, Fill)
              ]
              Items (List.indexed model.msgs)
            ]    
          ]          

          yield button [
            Text "Quit"
            OnClicked (fun _ -> dispatch Model.Quit)
            Styles [
              Pos (AbsPos 0, Position.PercentPos 99.0)
            ]
          ]
    ]
    ]
