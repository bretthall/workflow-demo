module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
// MODEL

type Model = int

type Msg =
| Increment
| Decrement

let init() : Model = 
  match BrowserLocalStorage.load Thoth.Json.Decode.int "value" with
  | Ok value -> value
  | Error _ -> 0

// UPDATE

let update (msg:Msg) (model:Model) =
  let newModel =   
    match msg with
    | Increment -> model + 3
    | Decrement -> model - 2    
  BrowserLocalStorage.save "value" newModel
  newModel
// VIEW (rendered with React)

let view (model:Model) dispatch =

  div []
      [ button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        div [] [ str (string model) ]
        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] ]

// App
Program.mkSimple init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
