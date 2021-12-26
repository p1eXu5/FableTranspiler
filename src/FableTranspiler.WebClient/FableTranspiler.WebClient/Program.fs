module Program

open Fable.Core.JsInterop
open FSharp.Data


importAll "./assets/App.css"


open Elmish
open Elmish.React
open App

Program.mkProgram Model.Init update (fun state dispatch -> app {| Model = state; Dispatch = dispatch |})
|> Program.withReactSynchronous "root"
|> Program.withConsoleTrace
|> Program.run