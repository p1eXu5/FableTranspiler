namespace FableTranspiler.Adapters.WpfClient

open Elmish
open Microsoft.Extensions.Logging
open FableTranspiler.SimpleTypes
open FableTranspiler.Ports.Persistence
open FableTranspiler.Ports.PortsBuilder
open FableTranspiler.Ports.AsyncPortsBuilder
open Microsoft.Win32
open FableTranspiler.Domain.UseCases
open FableTranspiler.Domain.UseCases.Implementation
open FsToolkit.ErrorHandling
open FableTranspiler.Adapters.WpfClient.Components

type MainModel =
    {
        ProcessingFile : FullPath option
        ModuleTreeList : ModuleTreeList
        DtsModule : DtsModule
        //FileTree: FileTreeViewModel list option
        //SelectedModuleKey: string list option
        //SelectedDocument: FileTreeViewModel option
        //DtsModules : Map<string list, DtsStatementViewModel list>
        //FsModules : Map<string list, FsStatementViewModel list>
        //SelectedModuleKey : string list
        LastError : string option
    }


module internal MainModel =

    open Elmish.Extensions

    type Msg =
        | OpenFile
        | ModuleTreeListMsg of ModuleTreeList.Msg
        | DtsModuleMsg of DtsModule.Msg
        | ParseFile of Operation<FullPath, FullPathTree>
        | SetSelectedModule of string list option


    let init () =
        fun () ->
            let (moduleTreeList, moduleTreeListMsg) = ModuleTreeList.init ()
            let (dtsModule, dtsModuleMsg) = DtsModule.init ()
            {
                ProcessingFile = None
                ModuleTreeList = moduleTreeList
                DtsModule = dtsModule
                //FsModules = Map.empty
                //SelectedModuleKey = []
                LastError = None
            },
            Cmd.batch [
                Cmd.map ModuleTreeListMsg moduleTreeListMsg
                Cmd.map DtsModuleMsg dtsModuleMsg
            ]


    let update (msg: Msg) (model: MainModel) =
        ports {
            match msg with
            | OpenFile ->
                let fd = OpenFileDialog()
                fd.Filter <- "d.ts files (*.d.ts)|*.d.ts|All files (*.*)|*.*"
                let result = fd.ShowDialog()
                if result.HasValue && result.Value then
                    return
                        fd.FileName 
                        |> FullPath.Create
                        |> Result.map (fun fp -> 
                            ( { model with ProcessingFile = fp |> Some },
                              Cmd.ofMsg (Start fp |> ParseFile) )
                        )
                        |> Result.defaultValue (model, Cmd.none)
                else 
                    return (model, Cmd.none)

            | ParseFile (Operation.Start fp) ->
                let! (config: StatementStore * ReadFileAsync) = Ports.ask
                return
                    {
                        model with
                            LastError = None
                    },
                    Cmd.OfTask.perform (AsyncPorts.run config) (parseFile fp) (Operation.Finish >> ParseFile)
    
            | ParseFile (Operation.Finish fullPathTree) ->
                return
                    {model with ProcessingFile = None}
                    , Cmd.ofMsg (fullPathTree |> ModuleTreeList.Msg.AppendNewModuleTree |> ModuleTreeListMsg )
    
    
            | ModuleTreeListMsg msg ->
                let (mtlModel, mtlMsg) = ModuleTreeList.update msg model.ModuleTreeList
                let model' = {model with ModuleTreeList = mtlModel}
                match msg with
                | ModuleTreeList.SelectModule mtlModel fullPath ->
                    return
                        model',
                        Cmd.batch [
                            Cmd.map ModuleTreeListMsg mtlMsg
                            Cmd.ofMsg (DtsModuleMsg (DtsModule.Msg.Interpret fullPath))
                        ]
                | _ ->
                    return 
                        model'
                        , Cmd.map ModuleTreeListMsg mtlMsg
    
            | DtsModuleMsg msg ->
                let! (dtsModule', dtsModuleMsg) =
                    DtsModule.update msg model.DtsModule
                    |> Ports.withEnv (fun config -> fst config)

                return
                    {model with DtsModule = dtsModule'}, Cmd.map DtsModuleMsg dtsModuleMsg

            | _ -> return model, Cmd.none
        }


    // =========================================================

    open Elmish.WPF

    let bindings () =
        [
            "OpenFileCommand" |> Binding.cmd (fun _ -> OpenFile)
    
            "ModuleTreeList" 
            |> Binding.SubModel.required ModuleTreeList.bindings
            |> Binding.mapModel (fun m -> m.ModuleTreeList)
            |> Binding.mapMsg ModuleTreeListMsg

            "DtsModule"
            |> Binding.SubModel.required DtsModule.bindings
            |> Binding.mapModel (fun m -> m.DtsModule)
            |> Binding.mapMsg DtsModuleMsg
    
            "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
        ]