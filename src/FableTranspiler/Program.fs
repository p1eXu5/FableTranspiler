module FableTranspiler.Program

open Types
open Infrastruture
open Elmish
open FableTranspiler.VmAdapters


let update (msg: Msg) (model: Model) =
    match msg with
    | SelectFile o ->
        match o with
        | :? FileTreeViewModel as fileTree ->
            match fileTree.StatementsResult.Statements with
            | Ok s -> 
                { 
                    model with 
                        SelectedModule = s |> Some
                        LastError = None
                }, Cmd.none

            | Error err -> 
                { 
                    model with 
                        SelectedModule = None
                        LastError = err |> Some
                }, Cmd.none
        | _ -> failwith "Unknown type SelectFile message"


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