namespace FableTranspiler.Components

open FableTranspiler.VmAdapters

type ModuleTreeListViewModel =
    {
        ModuleTreeList: ModuleTreeViewModel list
        SelectedModuleKey: string list
    }

[<RequireQualifiedAccess>]
module internal ModuleTreeListViewModel =

    type Msg =
        | SelectModule of SelectModuleMsg
        | DtsStatementMsg of int * DtsStatementViewModel.Msg
        | FsStatementMsg of int * FsStatementViewModel.Msg
    and
        /// for tree item selection:
        SelectModuleMsg =
            | ToggleModuleSelection of bool
            | ChildMsg of string list * SelectModuleMsg


    open Elmish
    open Elmish.WPF

    let init () =
        {
            ModuleTreeList = []
            SelectedModuleKey = []
        }
        , Cmd.none



    let changeIsSelected v model =
        { model with IsSelected = v}


    let rec tryTransformModule (key: string list) modules transform =
    
        let rec tryToggleIsSelected (key: string list) modules accum =
            match key with
            | head :: [] ->
                modules |> List.map (fun it -> 
                    if it.Key[0] = head then
                        transform it
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



    let update store msg model =
        match msg with
        | SelectModule (ChildMsg (key, ToggleModuleSelection v)) when not v ->
            {
                model with
                    ModuleTreeList = tryTransformModule key model.ModuleTreeList (changeIsSelected v)
                    SelectedModuleKey = []
            }
            , Cmd.none

        | SelectModule (ChildMsg (key, ToggleModuleSelection v)) when v ->

            match tryFindModule2 key model.ModuleTreeList with
            | Some module' when module'.DtsDocumentVm |> Option.isNone ->
                let module'' = ModuleTreeViewModel.produceDocuments module' store
                {
                    model with
                        ModuleTreeList = tryTransformModule key model.ModuleTreeList (fun _ -> module'' )
                        SelectedModuleKey = key
                }
                , Cmd.none

            | _ -> model, Cmd.none

        | _ -> model, Cmd.none



    // https://github.com/elmish/Elmish.WPF/blob/master/TUTORIAL.md#level-3-separate-message-type-and-arbitrary-customization-of-model-for-sub-bindings
    let rec parentBindings () : Binding<ModuleTreeViewModel, SelectModuleMsg> list = [
        "IsSelected" |> Binding.twoWay (
            (fun (m) -> m.IsSelected),
            (fun v -> ToggleModuleSelection v)
        )
        "FileName" |> Binding.oneWay (fun (vm) -> vm.FileName)
        "SubModules" |> Binding.subModelSeq (
            (fun m -> m.SubModules),
            (fun (m, sm) -> sm),
            (fun m -> m.Key),
            (fun (id, msg) -> 
                match msg with
                | ToggleModuleSelection _ -> ChildMsg (id, msg)
                | msg' -> msg'
            ),
            parentBindings
        )
    ]


    let selectedDts model =
        match model.SelectedModuleKey with
        | [] -> None
        | key ->
            tryFindModule2 key model.ModuleTreeList
            |> Option.bind (fun d -> d.DtsDocumentVm)


    let selectedFs model =
        match model.SelectedModuleKey with
        | [] -> None
        | key ->
            tryFindModule2 key model.ModuleTreeList
            |> Option.bind (fun d -> d.FsDocumentVm)


    let bindings () =
        [
            "ModuleTreeList" |> Binding.subModelSeq (
                (fun m -> m.ModuleTreeList),
                (fun (m, sm) -> sm),
                (fun (vm: ModuleTreeViewModel) -> vm.Key),
                (fun (id, msg) -> 
                    match msg with
                    | ToggleModuleSelection _ -> ChildMsg (id, msg) |> SelectModule
                    | msg' -> msg' |> SelectModule
                ),
                parentBindings
            )

            "SelectedDtsStatements" |> Binding.subModelSeq (
                (
                    fun m -> 
                        selectedDts m
                        |> Option.bind (fun dvm ->
                            match dvm with
                            | Choice1Of2 xvm -> xvm |> Some
                            | Choice2Of2 _ -> None
                        )
                        |> Option.defaultValue []
                ),
                (fun (_, sm) -> sm),
                (fun (vm) -> vm.DtsStatement.Index),
                DtsStatementMsg,
                DtsStatementViewModel.bindings
            )

            "SelectedDtsStatementsError" |> Binding.oneWayOpt(fun m -> 
                selectedDts m
                |> Option.bind (fun dvm ->
                    match dvm with
                    | Choice1Of2 _ -> None
                    | Choice2Of2 err -> err |> Some
                )
            )

            "SelectedFsStatements" |> Binding.subModelSeq (
                (
                    fun m -> 
                        selectedFs m
                        |> Option.bind (fun dvm ->
                            match dvm with
                            | Choice1Of2 xvm -> xvm |> Some
                            | Choice2Of2 _ -> None
                        )
                        |> Option.defaultValue []
                ),
                (fun (_, sm) -> sm),
                (fun (vm) -> vm.FsStatement.Index),
                FsStatementMsg,
                FsStatementViewModel.bindings
            )

            "SelectedFsStatementsError" |> Binding.oneWayOpt(fun m -> 
                selectedFs m
                |> Option.bind (fun dvm ->
                    match dvm with
                    | Choice1Of2 _ -> None
                    | Choice2Of2 err -> err |> Some
                )
            )
        ]