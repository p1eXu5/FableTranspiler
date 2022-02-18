namespace FableTranspiler.Adapters.WpfClient.Components

open FableTranspiler
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters
open Microsoft.Extensions.Logging
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Domain.UseCases


[<ReferenceEquality>]
type ModuleTree =
    {
        RootPath : FullPath
        ModulePath : FullPath
        /// fileName :: parent
        Key : string list
        SubModules : ModuleTree list
        IsSelected : bool
    }

type ModuleTreeList = ModuleTree list


[<RequireQualifiedAccess>]
module internal ModuleTreeList =

    type Msg =
        | AppendNewModuleTree of FullPathTree
        | ResetSelection
        | ChangeSelection of RootFullPath: FullPath * SelectModuleMsg
    and
        /// for tree item selection:
        SelectModuleMsg =
            | ToggleModuleSelection of bool
            | ChildMsg of string list * SelectModuleMsg


    open Elmish
    open Elmish.WPF

    let init () : ModuleTreeList * Cmd<Msg> =
        [], Cmd.none


    [<AutoOpen>]
    module internal ModuleTree =

        open System.IO

        let toModuleTree rootFullPath =
            let getModuleName fp =
                Path.GetFileName(fp |> FullPath.Value)[..^5]
            (
                (fun parentKey fp ->
                    let key = (getModuleName fp) :: parentKey
                    {
                        RootPath = rootFullPath
                        ModulePath = fp
                        Key = key
                        SubModules = []
                        IsSelected = false
                    }
                ),
                (fun (m: ModuleTree) sm ->
                    {m with SubModules = sm}
                ),
                (fun m _ -> m),
                [],
                (fun m -> m.Key)
            )

        let changeIsSelected v (model: ModuleTree) =
            { model with IsSelected = v}

        let rec tryTransformModule rootFullPath key modules transform =
    
            let rec tryToggleIsSelected key modules accum =
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
    

            modules
            |> List.map (fun moduleTree ->
                if rootFullPath = moduleTree.RootPath then
                    tryToggleIsSelected (key |> List.rev) [moduleTree] []
                    |> List.head
                else moduleTree
            )

        let tryFindModule2 rootFullPath key modules : ModuleTree option =
    
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
    

            modules
            |> List.tryFind (fun mt -> mt.RootPath = rootFullPath)
            |> Option.bind (fun mt ->
                tryFindModule (key |> List.rev) [mt] []
            )

        let rec allModules moduleTree =
            moduleTree
            :: (
                moduleTree.SubModules
                |> List.map allModules
                |> List.concat
            )


    let (|SelectModule|_|) moduleList = function
        | ChangeSelection (rootFullPath, ChildMsg (key, ToggleModuleSelection v)) when v = true ->
            tryFindModule2 rootFullPath key moduleList
            |> Option.map (fun mt -> mt.ModulePath)
        | _ -> None



    // ==============
    //    Program
    // ==============

    let update msg model =
        match msg with
        | AppendNewModuleTree fullPathTree ->
            let rootFullPath = fullPathTree |> FullPathTree.fullPath
            let moduleTree = fullPathTree |> FullPathTree.apply (ModuleTree.toModuleTree rootFullPath)
            moduleTree :: model
            , Cmd.batch [
                Cmd.ofMsg ResetSelection
                Cmd.ofMsg (ChangeSelection (rootFullPath, (ChildMsg (moduleTree.Key, ToggleModuleSelection true))))
            ]
            
        | ChangeSelection (rootFullPath, ChildMsg (key, ToggleModuleSelection v)) ->
            let newModel = ModuleTree.tryTransformModule rootFullPath key model (ModuleTree.changeIsSelected v)
            newModel
            , Cmd.none

        | _ -> model, Cmd.none


    // ==============
    //    Bindings
    // ==============

    open System.IO

    // https://github.com/elmish/Elmish.WPF/blob/master/TUTORIAL.md#level-3-separate-message-type-and-arbitrary-customization-of-model-for-sub-bindings
    let rec moduleTreeBindings () : Binding<ModuleTree, SelectModuleMsg> list = [
        "IsSelected" |> Binding.twoWay (
            (fun (m) -> m.IsSelected),
            (fun v -> ToggleModuleSelection v)
        )
        "FileName" |> Binding.oneWay (fun (m) -> Path.GetFileName(m.ModulePath |> FullPath.Value))
        "SubModules" |> Binding.subModelSeq (
            (fun m -> m.SubModules),
            (fun (m, sm) -> sm),
            (fun m -> m.Key),
            (fun (id, msg) -> 
                match msg with
                | ToggleModuleSelection _ -> ChildMsg (id, msg)
                | _ -> msg
            ),
            moduleTreeBindings
        )
    ]


    let bindings () =
        [
            "ModuleTreeList" |> Binding.subModelSeq (
                (fun m -> m),
                (fun (m, sm) -> sm),
                (fun (vm: ModuleTree) -> (vm.RootPath, vm.Key)),
                (fun (id, msg) ->
                    match msg with
                    | ToggleModuleSelection _ -> (fst id, ChildMsg (snd id, msg))
                    | _ -> (fst id, msg)
                    |> ChangeSelection
                ),
                moduleTreeBindings
            )

            //"SelectedDtsStatements" |> Binding.oneWayOpt (
            //    fun m -> 
            //        m.DtsStatements
            //        |> Option.bind (fun dvm ->
            //            match dvm with
            //            | Choice1Of2 xvm -> xvm |> Some
            //            | Choice2Of2 _ -> None
            //        )
            //)

            //"SelectedDtsStatementsError" |> Binding.oneWayOpt(fun m -> 
            //    m.DtsStatements
            //    |> Option.bind (fun dvm ->
            //        match dvm with
            //        | Choice1Of2 _ -> None
            //        | Choice2Of2 err -> err |> Some
            //    )
            //)

            //"SelectedFsStatements" |> Binding.oneWayOpt (
            //    fun m -> 
            //        m.FsStatements
            //        |> Option.bind (fun dvm ->
            //            match dvm with
            //            | Choice1Of2 xvm -> xvm |> Some
            //            | Choice2Of2 _ -> None
            //        )
            //)

            //"SelectedFsStatementsError" |> Binding.oneWayOpt(
            //    fun m -> 
            //        m.FsStatements
            //        |> Option.bind (fun dvm ->
            //            match dvm with
            //            | Choice1Of2 _ -> None
            //            | Choice2Of2 err -> err |> Some
            //        )
            //)
        ]