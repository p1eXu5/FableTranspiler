namespace FableTranspiler.Adapters.WpfClient

open Elmish
open Microsoft.Extensions.Logging
open FableTranspiler.SimpleTypes
open FableTranspiler.Components
open FableTranspiler.AppTypes
open FableTranspiler
open Microsoft.Win32


type MainModel =
    {
        ProcessingFile : string option
        //FileTree: FileTreeViewModel list option
        //SelectedModuleKey: string list option
        //SelectedDocument: FileTreeViewModel option
        ModuleTreeList : ModuleTreeCollection
        DtsModules : Map<string list, DtsStatementViewModel list>
        FsModules : Map<string list, FsStatementViewModel list>
        SelectedModuleKey : string list
        LastError : string option
    }


module internal MainModel =

    open Elmish.Extensions

    type Msg =
        | OpenFile
        | ModuleTreeListMsg of ModuleTreeCollection.Msg
        | ParseFile of Operation<Result<(LibLocation * FileParsingResultTree), string>>
        | SetSelectedModule of string list option


    let init loggerFactory =
        fun () ->
            let (moduleTree, msg) = ModuleTreeCollection.init loggerFactory
            {
                ProcessingFile = None
                ModuleTreeList = moduleTree
                DtsModules = Map.empty
                FsModules = Map.empty
                SelectedModuleKey = []
                LastError = None
            },
            Cmd.map ModuleTreeListMsg msg


    let update (msg: Msg) (model: MainModel) =
        match msg with
        | OpenFile ->
            let fd = OpenFileDialog()
            fd.Filter <- "d.ts files (*.d.ts)|*.d.ts|All files (*.*)|*.*"
            let result = fd.ShowDialog()
            if result.HasValue && result.Value then (model, Cmd.ofMsg (Start |> ParseFile))
            else (model, Cmd.none)

        | ParseFile (Operation.Start) -> 
            {
                model with
                    LastError = None
            },
            Cmd.OfTask.perform Infrastruture.openAndProcessFile () (Operation.Finish >> ParseFile)
    
        | ParseFile (Operation.Finish result) ->
            match result with
            | Ok (modulePath, parsingResultTree) ->

                let (moduleTreeList, moduleTreeListMsg) = model.ModuleTreeList |> ModuleTreeCollection.add modulePath parsingResultTree

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
    
        | ModuleTreeListMsg msg ->
            let (model', msg') = ModuleTreeCollection.update msg model.ModuleTreeList
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
            "OpenFileCommand" |> Binding.cmd (fun m -> Operation.Start |> ParseFile)
    
            "ModuleTreeListVm" |> Binding.subModel(
                (fun m -> m.ModuleTreeList),
                (fun (_, sm) -> sm),
                ModuleTreeListMsg,
                ModuleTreeCollection.bindings
            )
    
            "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
        ]