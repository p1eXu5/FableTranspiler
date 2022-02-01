module internal FableTranspiler.Program

open Infrastruture
open Elmish
open FableTranspiler.MainModel
open FableTranspiler.VmAdapters
open FableTranspiler.Components


let tryFindModule2 (key: string list) modules : ModuleTreeViewModel option =

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


let update store (msg: Msg) (model: MainModel) =
    match msg with
    | ParseFile -> 
        {
            model with
                IsBusy = true
                LastError = None
        },
        Cmd.OfTask.either openFile () FileParsed Failed

    | FileParsed (Ok moduleTree) ->
        let fileTree =  moduleTree |> ModuleTreeViewModel.toFileTreeVm store [] true
        {
            model with 
                ModuleTreeList = 
                    {
                        model.ModuleTreeList with
                            ModuleTreeList = fileTree :: model.ModuleTreeList.ModuleTreeList
                            SelectedModuleKey = fileTree.Key
                    }
                IsBusy = false
        }, Cmd.none

    | Failed e ->
        {
            model with 
                IsBusy = false
                LastError = e.Message |> Some
        }
        , Cmd.none

    | ModuleTreeMsg m ->
        let (model', msg') = ModuleTreeListViewModel.update store m model.ModuleTreeList
        {
            model with
                ModuleTreeList = model'
        }
        , Cmd.map ModuleTreeMsg msg'

    //| SetSelectedModule key -> {model with SelectedModuleKey = key}, Cmd.none

    //| ChildMsg (key, ToggleModuleSelection v) ->
    //    match model.FileTree with
    //    | Some fileTree ->
    //        { 
    //            model with 
    //                FileTree = tryToggleIsSelected2 key fileTree v |> Some 
    //                SelectedModuleKey = key |> Some
    //        }
    //        , Cmd.none
    //    | _ -> model, Cmd.none



    | _ -> {model with IsBusy = false}, Cmd.none