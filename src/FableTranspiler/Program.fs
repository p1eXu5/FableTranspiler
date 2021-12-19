module FableTranspiler.Program

open Types
open Infrastruture
open Elmish


let update (msg: Msg) (model: Model) =
    match msg with
    | ParseFile -> 
        {
            model with
                IsBusy = true
        },
        Cmd.OfTask.either openFile () FileParsed Failed

    | FileParsed (Ok moduleTree) ->
        {
            model with 
                ModuleTree = moduleTree |> Some
                IsBusy = false
        }, Cmd.none

    | _ -> {model with IsBusy = false}, Cmd.none