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
                { 
                    model with 
                        SelectedDocument = fileTree |> Some
                        LastError = None
                }, Cmd.none
        | _ -> failwith "Unknown type SelectFile message"

    | ParseFile -> 
        {
            model with
                IsBusy = true
        },
        Cmd.OfTask.either openFile () FileParsed Failed

    | FileParsed (Ok moduleTree) ->
        let fileTree =  moduleTree |> FileTree.toFileTreeVm |> List.singleton |> Some
        {
            model with 
                FileTree = fileTree
                SelectedDocument = fileTree |> Option.map (fun l -> l |> List.head)
                IsBusy = false
        }, Cmd.none


    | _ -> {model with IsBusy = false}, Cmd.none