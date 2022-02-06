namespace FableTranspiler

open Elmish
open AppTypes
open FableTranspiler.VmAdapters
open FableTranspiler.Components
open FableTranspiler.Parsers.Types
open SimpleTypes



type MainModel =
    {
        //FileTree: FileTreeViewModel list option
        //SelectedModuleKey: string list option
        //SelectedDocument: FileTreeViewModel option
        ModuleTreeList: ModuleTreeCollection
        DtsModules: Map<string list, DtsStatementViewModel list>
        FsModules: Map<string list, FsStatementViewModel list>
        SelectedModuleKey: string list
        IsBusy: bool
        LastError: string option
    }


module internal MainModel =

    open Elmish.Extensions

    type Msg =
        | ModuleTreeListMsg of ModuleTreeList.Msg
        | ParseFile of AsyncOperationMsg<Result<(LibLocation * AppTypes.FileParsingResultTree), string>>
        | SetSelectedModule of string list option


    let init () =
        let (moduleTree, msg) = ModuleTreeList.init ()
        {
            ModuleTreeList = moduleTree
            DtsModules = Map.empty
            FsModules = Map.empty
            SelectedModuleKey = []
            IsBusy = false
            LastError = None
        },
        Cmd.map ModuleTreeListMsg msg


    let update store (msg: Msg) (model: MainModel) =
        match msg with
        | ParseFile (AsyncOperationMsg.Start) -> 
            {
                model with
                    IsBusy = true
                    LastError = None
            },
            Cmd.OfTask.perform Infrastruture.openAndProcessFile () (AsyncOperationMsg.Finish >> ParseFile)
    
        | ParseFile (AsyncOperationMsg.Finish result) ->
            match result with
            | Ok (modulePath, parsingResultTree) ->
                let (moduleTreeList, moduleTreeListMsg) =
                    model.ModuleTreeList
                    |> ModuleTreeList.add store modulePath parsingResultTree


                //parsingResultTree |> ModuleTreeViewModel.toFileTreeVm store [] true
                {
                    model with 
                        ModuleTreeList = moduleTreeList
                        IsBusy = false
                }, Cmd.map ModuleTreeListMsg moduleTreeListMsg
    
            | Error err ->
                {
                    model with 
                        IsBusy = false
                        LastError = err |> Some
                }
                , Cmd.none
    
        | ModuleTreeListMsg m ->
            let (model', msg') = ModuleTreeList.update store m model.ModuleTreeList
            {
                model with
                    ModuleTreeList = model'
            }
            , Cmd.map ModuleTreeListMsg msg'
    
    
        | _ -> {model with IsBusy = false}, Cmd.none


    // =========================================================

    open Elmish.WPF

    let bindings () =
        [
            "OpenFileCommand" |> Binding.cmd (fun m -> AsyncOperationMsg.Start |> ParseFile)
    
            "ModuleTreeListVm" |> Binding.subModel(
                (fun m -> m.ModuleTreeList),
                (fun (_, sm) -> sm),
                ModuleTreeListMsg,
                ModuleTreeList.bindings
            )
    
            "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
        ]