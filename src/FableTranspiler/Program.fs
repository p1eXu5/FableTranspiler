module FableTranspiler.Program

open Types
open Infrastruture
open Elmish
open FableTranspiler.VmAdapters


let rec tryFindModule offset (key: string list) modules : FileTreeViewModel option =

    let level = key.Length - offset

    if level = 0 then None
    elif level = 1 then modules |> List.tryFind (fun it -> it.Key = key)
    else 
        modules
        |> List.map (fun it -> 
            tryFindModule (offset + 1) key it.SubModules
        )
        |> List.choose id
        |> List.tryHead


let rec tryToggleIsSelected offset (key: string list) modules v =

    let level = key.Length - offset
    
    if level = 0 then []
    elif level = 1 then 
        modules
        |> List.map (fun it -> 
            if it.Key = key then
                {it with IsSelected = v}
            else
                it
        )
    else 
        modules
        |> List.map (fun it -> 
            {it with SubModules = tryToggleIsSelected (offset + 1) key it.SubModules v}
        )



let update (msg: Msg) (model: Model) =
    match msg with
    //| SelectFile o ->
    //    match o with
    //    | :? FileTreeViewModel as fileTree ->
    //            { 
    //                model with 
    //                    SelectedDocument = fileTree |> Some
    //                    LastError = None
    //            }, Cmd.none
    //    | _ -> failwith "Unknown type SelectFile message"

    | ParseFile -> 
        {
            model with
                IsBusy = true
                LastError = None
        },
        Cmd.OfTask.either openFile () FileParsed Failed

    | FileParsed (Ok moduleTree) ->
        let fileTree =  moduleTree |> FileTree.toFileTreeVm [] true |> List.singleton |> Some
        {
            model with 
                FileTree = fileTree
                SelectedModuleKey = fileTree |> Option.map (fun l -> (l |> List.head).Key)
                IsBusy = false
        }, Cmd.none

    | SetSelectedModule key -> {model with SelectedModuleKey = key}, Cmd.none

    | ChildMsg (key, ToggleModuleSelection v) ->
        match model.FileTree with
        | Some fileTree ->
            { 
                model with 
                    FileTree = tryToggleIsSelected 0 key fileTree v |> Some 
                    SelectedModuleKey = key |> Some
            }
            , Cmd.none
        | _ -> model, Cmd.none


    | Failed e ->
        {
            model with 
                IsBusy = false
                LastError = e.Message |> Some
        }
        , Cmd.none

    | _ -> {model with IsBusy = false}, Cmd.none