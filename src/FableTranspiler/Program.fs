module FableTranspiler.Program

open Types
open Infrastruture
open Elmish
open FableTranspiler.VmAdapters


let tryFindModule2 (key: string list) modules : FileTreeViewModel option =

    let rec tryFindModule (key: string list) modules accum =

        match key with
        | head :: [] ->
            modules |> List.tryFind (fun it -> it.Key[0] = head)
        | head :: tail ->

            let accum' = head :: accum

            modules
            |> List.tryFind (fun it -> it.Key = accum')
            |> Option.bind (fun it ->
                tryFindModule tail it.SubModules accum'
            )
        | [] -> None

    tryFindModule (key |> List.rev) modules []



let rec tryToggleIsSelected2 (key: string list) modules v =

    let rec tryToggleIsSelected (key: string list) modules accum =
        match key with
        | head :: [] ->
            modules |> List.map (fun it -> 
                if it.Key[0] = head then
                    {it with IsSelected = v}
                else
                    it
            )
        | head :: tail ->

            let accum' = head :: accum

            modules 
            |> List.map (fun it -> 
                if it.Key = accum' then
                    {it with SubModules = tryToggleIsSelected tail it.SubModules accum'}
                else
                    it
            )

        | [] -> []

    tryToggleIsSelected (key |> List.rev) modules []


let update (msg: Msg) (model: Model) =
    match msg with
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

    | Failed e ->
        {
            model with 
                IsBusy = false
                LastError = e.Message |> Some
        }
        , Cmd.none

    | SetSelectedModule key -> {model with SelectedModuleKey = key}, Cmd.none

    | ChildMsg (key, ToggleModuleSelection v) ->
        match model.FileTree with
        | Some fileTree ->
            { 
                model with 
                    FileTree = tryToggleIsSelected2 key fileTree v |> Some 
                    SelectedModuleKey = key |> Some
            }
            , Cmd.none
        | _ -> model, Cmd.none



    | _ -> {model with IsBusy = false}, Cmd.none