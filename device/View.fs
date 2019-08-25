module WorkflowDemo.Device.View

open Terminal.Gui
open Terminal.Gui.Elmish
open WorkflowDemo.Common

type WorkflowName = string

module private Private = 

  let stateToText = function
    | Device.Good -> "Good"
    | Device.Bad -> "Bad"
    | Device.Ugly -> "Ugly"

  let stateToColor = function
    | Device.Good -> Color.Green
    | Device.Bad -> Color.Red
    | Device.Ugly -> Color.Brown

open Private

let view (model: Model.Model) dispatch =

  page [
    window [
        Title "Device"
        Styles [Colors (Color.BrightGreen, Color.Black)]        
    ][
      let stateRow = 0
      yield label [
        Styles [
          Colors (stateToColor model.state, Color.Blue)
          Pos (AbsPos 0, AbsPos stateRow)
        ]
        Text (sprintf "State: %s" (stateToText model.state))                
      ]
      
      let dataRow = stateRow + 1
      let dataLabel = sprintf "Data: %d" model.dataValue
      yield label [
        Text dataLabel
        Styles [Pos (AbsPos 0, AbsPos dataRow)]            
      ]
      yield button [
        Text "Reset"
        Styles [Pos (AbsPos (max ("Data:      ".Length) dataLabel.Length), AbsPos dataRow)]            
        OnClicked (fun _ -> dispatch (Model.DataMsg Model.ResetData))
      ]
      
      let inputRow = dataRow + 1
      match model.inputState with
      | Some inputState -> 
        let inputLabel = label [
          Styles [Pos (AbsPos 0, AbsPos inputRow)]
          Text inputState.prompt
        ]
        yield inputLabel
        yield textField [
          Value inputState.current
          OnChanged (fun v -> dispatch (Model.InputUpdate v |> Model.InputMsg))
          Styles [
            Pos (AbsPos inputState.prompt.Length, AbsPos inputRow)
            Dim (AbsDim 30, AbsDim 1)
          ]
        ]
        yield button [
          Text "Send"
          OnClicked (fun _ -> dispatch (Model.InputDone |> Model.InputMsg))
          Styles [
            Pos (AbsPos (inputState.prompt.Length + 30), AbsPos inputRow)
          ]
        ]
      | None -> yield! []
      let msgsRow = inputRow + 1
      yield frameView [
        Text "Messages"
        Styles [
          Pos (AbsPos 0, AbsPos msgsRow)
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
