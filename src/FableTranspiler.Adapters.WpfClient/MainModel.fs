namespace FableTranspiler.Adapters.WpfClient

open Elmish
open Microsoft.Extensions.Logging
open FableTranspiler.SimpleTypes
open FableTranspiler.Components
open FableTranspiler.Ports.Persistence
open FableTranspiler.Ports.AsyncPortsBuilder
open Microsoft.Win32
open FableTranspiler.Domain.UseCases
open FableTranspiler.Domain.UseCases.Implementation
open FsToolkit.ErrorHandling

type MainModel =
    {
        ProcessingFile : FullPath option
        UriGraph : FullPathTree option
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
        | ParseFile of Operation<FullPath, FullPathTree>
        | SetSelectedModule of string list option


    let init loggerFactory =
        fun () ->
            let (moduleTree, msg) = ModuleTreeCollection.init loggerFactory
            {
                ProcessingFile = None
                UriGraph = None
                ModuleTreeList = moduleTree
                DtsModules = Map.empty
                FsModules = Map.empty
                SelectedModuleKey = []
                LastError = None
            },
            Cmd.map ModuleTreeListMsg msg


    let update (msg: Msg) (model: MainModel) =
        taskPorts {
            match msg with
            | OpenFile ->
                let fd = OpenFileDialog()
                fd.Filter <- "d.ts files (*.d.ts)|*.d.ts|All files (*.*)|*.*"
                let result = fd.ShowDialog()
                if result.HasValue && result.Value then
                    return
                        fd.FileName 
                        |> FullPath.Create
                        |> Result.map (fun fp -> ({model with ProcessingFile = fp |> Some}, Cmd.ofMsg (Start fp |> ParseFile)))
                        |> Result.defaultValue (model, Cmd.none)
                else return (model, Cmd.none)

            | ParseFile (Operation.Start fp) ->
                let! (config: StatementStore * ReadFileAsync) = AsyncPorts.ask
                return
                    {
                        model with
                            LastError = None
                    },
                    Cmd.OfTask.perform (AsyncPorts.run config ) (parseFile fp) (Operation.Finish >> ParseFile)
    
            | ParseFile (Operation.Finish uriGraph) ->
                return
                    {
                        model with
                            ProcessingFile = None
                            UriGraph = uriGraph |> Some
                    }
                    , Cmd.none
    
    
            | ModuleTreeListMsg msg ->
                let (model', msg') = ModuleTreeCollection.update msg model.ModuleTreeList
                return 
                    {
                        model with
                            ModuleTreeList = model'
                    }
                    , Cmd.map ModuleTreeListMsg msg'
    
    
            | _ -> return model, Cmd.none
        }


    // =========================================================

    open Elmish.WPF

    let bindings () =
        [
            "OpenFileCommand" |> Binding.cmd (fun _ -> OpenFile)
    
            "ModuleTreeListVm" |> Binding.subModel(
                (fun m -> m.ModuleTreeList),
                (fun (_, sm) -> sm),
                ModuleTreeListMsg,
                ModuleTreeCollection.bindings
            )
    
            "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
        ]