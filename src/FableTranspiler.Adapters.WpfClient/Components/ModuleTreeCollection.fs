namespace FableTranspiler.Components

open FableTranspiler
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters
open Microsoft.Extensions.Logging
open FableTranspiler.Interpreters.FsInterpreter


[<ReferenceEquality>]
type ModuleTreeCollection =
    {
        LibModuleTreeMap: Map<LibLocation, ModuleTree>
        InterpretConfig: InterpretConfig
        DtsStatements: Choice<DtsStatementViewModel list, CodeItem list> option
        FsStatements: Choice<FsStatementViewModel list, CodeItem list> option
        SelectedModuleKey: (LibLocation * string list) option
    }

[<RequireQualifiedAccess>]
module internal ModuleTreeCollection =

    type Msg =
        | SelectModule of LibLocation * SelectModuleMsg
        | ResetSelection
        | DtsStatementMsg of int * DtsStatementViewModel.Msg
        | FsStatementMsg of int * FsStatementViewModel.Msg
    and
        /// for tree item selection:
        SelectModuleMsg =
            | ToggleModuleSelection of bool
            | ChildMsg of string list * SelectModuleMsg


    open Elmish
    open Elmish.WPF

    let init loggerFactory =
        {
            LibModuleTreeMap = Map.empty
            InterpretConfig = InterpretConfigFactory.build loggerFactory (fun _ -> None) FsCodeStyle.Fable
            DtsStatements = None
            FsStatements = None
            SelectedModuleKey = None
        }
        , Cmd.none


    let add libLocation parsingResultTree model =
        let moduleTree = ModuleTree.build libLocation parsingResultTree

        (* необходимо изменить InterpretConfig для включения в поиск добавляемое дерево
           поиск осуществляется по алгоритму
           определить количество уровней - '../..'
           в каждом дереве попытаться найти модуль, путь которого заканчивается на имя искомого модуля + '.d.ts'
        *)

        {
            model with
                LibModuleTreeMap = 
                    model.LibModuleTreeMap
                    |> Map.add libLocation moduleTree
        }
        , Cmd.batch [
            Cmd.ofMsg ResetSelection
            Cmd.ofMsg (SelectModule (libLocation, (ChildMsg (moduleTree.Key, ToggleModuleSelection true))))
        ]



    // ==============
    //    Program
    // ==============

    let changeIsSelected v model =
        { model with IsSelected = v}


    let rec tryTransformModule (key: LibLocation * string list) modules transform =
    
        let rec tryToggleIsSelected (key: string list) modules accum =
            match key with
            | head :: [] ->
                modules 
                |> List.map (fun it -> 
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
    
        let (modulePath, moduleKey) = key

        modules
        |> Map.map (fun key rootModule ->
            if key = modulePath then
                tryToggleIsSelected (moduleKey |> List.rev) [rootModule] []
                |> List.head
            else rootModule
        )


    let tryFindModule2 (key: LibLocation * string list) modules : ModuleTree option =
    
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
    
        let (modulePath, moduleKey) = key

        modules
        |> Map.tryFind modulePath
        |> Option.bind (fun m ->
            tryFindModule (moduleKey |> List.rev) [m] []
        )



    let update msg model =
        match msg with
        | SelectModule (modulePath, ChildMsg (key, ToggleModuleSelection v)) when v = false ->
            {
                model with
                    LibModuleTreeMap = tryTransformModule (modulePath, key) model.LibModuleTreeMap (changeIsSelected v)
                    SelectedModuleKey = None
            }
            , Cmd.none

        | SelectModule (modulePath, ChildMsg (key, ToggleModuleSelection v)) when v = true ->
            let selectedModuleKey = (modulePath, key)

            match tryFindModule2 selectedModuleKey model.LibModuleTreeMap with
            | Some selectedModule ->
                {
                    model with
                        LibModuleTreeMap = tryTransformModule selectedModuleKey model.LibModuleTreeMap (changeIsSelected v)
                        DtsStatements = selectedModule.DtsDocumentVm.Value |> Some
                        FsStatements = selectedModule.FsDocumentVm |> Interpreter.run model.InterpretConfig |> Some
                        SelectedModuleKey = selectedModuleKey |> Some
                }
                , Cmd.none

            | _ -> model, Cmd.none

        | _ -> model, Cmd.none


    // ==============
    //    Bindings
    // ==============


    // https://github.com/elmish/Elmish.WPF/blob/master/TUTORIAL.md#level-3-separate-message-type-and-arbitrary-customization-of-model-for-sub-bindings
    let rec parentBindings () : Binding<ModuleTree, SelectModuleMsg> list = [
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


    let bindings () =
        [
            "ModuleTreeList" |> Binding.subModelSeq (
                (fun m -> m.LibModuleTreeMap |> Map.values),
                (fun (m, sm) -> sm),
                (fun (vm: ModuleTree) -> vm.RootKey, vm.Key),
                (fun (id, msg) -> 
                    match msg with
                    | ToggleModuleSelection _ -> (fst id, ChildMsg (snd id, msg)) |> SelectModule
                    | msg' -> (fst id, msg') |> SelectModule
                ),
                parentBindings
            )

            "SelectedDtsStatements" |> Binding.oneWayOpt (
                fun m -> 
                    m.DtsStatements
                    |> Option.bind (fun dvm ->
                        match dvm with
                        | Choice1Of2 xvm -> xvm |> Some
                        | Choice2Of2 _ -> None
                    )
            )

            "SelectedDtsStatementsError" |> Binding.oneWayOpt(fun m -> 
                m.DtsStatements
                |> Option.bind (fun dvm ->
                    match dvm with
                    | Choice1Of2 _ -> None
                    | Choice2Of2 err -> err |> Some
                )
            )

            "SelectedFsStatements" |> Binding.oneWayOpt (
                fun m -> 
                    m.FsStatements
                    |> Option.bind (fun dvm ->
                        match dvm with
                        | Choice1Of2 xvm -> xvm |> Some
                        | Choice2Of2 _ -> None
                    )
            )

            "SelectedFsStatementsError" |> Binding.oneWayOpt(
                fun m -> 
                    m.FsStatements
                    |> Option.bind (fun dvm ->
                        match dvm with
                        | Choice1Of2 _ -> None
                        | Choice2Of2 err -> err |> Some
                    )
            )
        ]