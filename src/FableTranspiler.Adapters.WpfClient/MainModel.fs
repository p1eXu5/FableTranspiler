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
open FableTranspiler.Parsers.Types
open Ookii.Dialogs.Wpf
open System.Windows
open Types
open System

type internal MainModel =
    {
        StatementStore: StatementStore< Statement >
        ReadFileAsync: ReadFileAsync
        ProcessingFile: FullPath option
        ModuleTreeList: ModuleTreeList
        DtsModule: Components.DtsModule
        FsModule: Components.FsModule
        //FileTree: FileTreeViewModel list option
        //SelectedModuleKey: string list option
        //SelectedDocument: FileTreeViewModel option
        //DtsModules : Map<string list, DtsStatementViewModel list>
        //FsModules : Map<string list, FsStatementViewModel list>
        //SelectedModuleKey : string list
        LastError : string option
        RootFolderToSave: string option
        SettingsManager : Types.ISettingsManager
    }


module internal MainModel =

    open Elmish.Extensions

    type Msg =
        | OpenFile
        | ModuleTreeListMsg of ModuleTreeList.Msg
        | DtsModuleMsg of DtsModule.Msg
        | FsModuleMsg of FsModule.Msg
        | ParseFile of Operation<FullPath, FullPathTree>
        | SetSelectedModule of string list option
        | ChooseRootFolderToSave
        | SetRootFolderToSave of string option


    let init statementStore readFileAsync (settingsManager : ISettingsManager) =
        let (moduleTreeList, moduleTreeListMsg) = ModuleTreeList.init ()
        let (dtsModule, dtsModuleMsg) = DtsModule.init statementStore
        let (fsModule, fsModuleMsg) = FsModule.init statementStore
        let s = 
            match (settingsManager.Load "RootFolderToSave") with
            | :? string as s when not (String.IsNullOrWhiteSpace(s)) -> s |> Some
            | _ -> None

        {
            StatementStore = statementStore
            ReadFileAsync = readFileAsync
            ProcessingFile = None
            ModuleTreeList = moduleTreeList
            DtsModule = dtsModule
            FsModule = fsModule
            LastError = None
            RootFolderToSave = s
            SettingsManager = settingsManager
        },
        Cmd.batch [
            Cmd.map ModuleTreeListMsg moduleTreeListMsg
            Cmd.map DtsModuleMsg dtsModuleMsg
            Cmd.map FsModuleMsg fsModuleMsg
        ]


    let update (msg: Msg) (model: MainModel) =
        match msg with
        | OpenFile ->
            let fd = OpenFileDialog()
            fd.Filter <- "d.ts files (*.d.ts)|*.d.ts|All files (*.*)|*.*"
            let result = fd.ShowDialog()
            if result.HasValue && result.Value then
                fd.FileName 
                |> FullPath.Create
                |> Result.map (fun fp -> 
                    ( { model with ProcessingFile = fp |> Some },
                        Cmd.ofMsg (Start fp |> ParseFile) )
                )
                |> Result.defaultValue (model, Cmd.none)
            else 
                (model, Cmd.none)

        | ParseFile (Operation.Start fp) ->
            {
                model with
                    LastError = None
            },
            Cmd.OfTask.perform (AsyncPorts.run (model.StatementStore, model.ReadFileAsync)) (parseFile fp) (Operation.Finish >> ParseFile)
    
        | ParseFile (Operation.Finish fullPathTree) ->
            {model with ProcessingFile = None}
            , Cmd.ofMsg (fullPathTree |> ModuleTreeList.Msg.AppendNewModuleTree |> ModuleTreeListMsg )
    
    
        | ModuleTreeListMsg msg ->
            let (mtlModel, mtlMsg) = ModuleTreeList.update msg model.ModuleTreeList
            let model' = {model with ModuleTreeList = mtlModel}

            match msg with
            | ModuleTreeList.SelectModule mtlModel (rootFullPath, moduleFullPath) when rootFullPath <> moduleFullPath ->
                model',
                Cmd.batch [
                    Cmd.map ModuleTreeListMsg mtlMsg
                    Cmd.ofMsg (DtsModuleMsg (DtsModule.Msg.Interpret moduleFullPath))
                    Cmd.ofMsg (FsModuleMsg (FsModule.Msg.Interpret (rootFullPath, moduleFullPath)))
                ]
            | _ ->
                model'
                , Cmd.map ModuleTreeListMsg mtlMsg
    
        | DtsModuleMsg msg ->
            let (dtsModule', dtsModuleMsg) =
                DtsModule.update msg model.DtsModule

            {model with DtsModule = dtsModule'}, Cmd.map DtsModuleMsg dtsModuleMsg

        | FsModuleMsg msg ->
            let (fsModule', fsModuleMsg) =
                FsModule.update msg model.FsModule

            {model with FsModule = fsModule'}, Cmd.map DtsModuleMsg fsModuleMsg

        | ChooseRootFolderToSave ->
            let dialog = VistaFolderBrowserDialog()
            dialog.Description <- "Choose root folder to save"
            dialog.UseDescriptionForTitle <- true
            if not VistaFolderBrowserDialog.IsVistaFolderDialogSupported then
                {model with LastError = "Dialog is not supported" |> Some}, Cmd.none
            else
                match dialog.ShowDialog() |> Option.ofNullable with
                | Some true ->
                    model.SettingsManager.Save "RootFolderToSave" dialog.SelectedPath
                    {model with RootFolderToSave = dialog.SelectedPath |> Some}, Cmd.none
                | _ -> model, Cmd.none

        | SetRootFolderToSave v ->
            v |> Option.iter (fun s -> model.SettingsManager.Save "RootFolderToSave"  s)
            {model with RootFolderToSave = v}, Cmd.none

        | _ -> model, Cmd.none


    // =====================
    //       Bindings
    // =====================
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

            "FsModule"
            |> Binding.SubModel.required FsModule.bindings
            |> Binding.mapModel (fun m -> m.FsModule)
            |> Binding.mapMsg FsModuleMsg
    
            "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)

            "RootFolderToSave" |> Binding.twoWayOpt ((fun m -> m.RootFolderToSave), SetRootFolderToSave)
            "ChooseRootFolderToSave" |> Binding.cmd ChooseRootFolderToSave
        ]