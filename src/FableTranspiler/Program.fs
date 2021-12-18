module FableTranspiler.Program

open Types
open Infrastruture
open Implementation
open Elmish


let update (msg: Msg) (model: Model) =
    match msg with
    | ParseFile -> 
        {
            model with
                IsBusy = true
        },
        Cmd.OfTask.either openFile () FileParsed Failed

    | FileParsed (Ok statements) ->
        {
            model with 
                SelectedModule = statements |> Some
                IsBusy = false
        }, Cmd.none

    | _ -> {model with IsBusy = false}, Cmd.none