module rec FableTranspiler.Interpreters.FsInterpreter.Facade

open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter.Common
open System
open FableTranspiler.SimpleTypes
open System.IO
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder
open Microsoft.Extensions.Logging
open System.Text
open FableTranspiler.Ports.PortsBuilder
open FsToolkit.ErrorHandling

let importAttribute name source =
    [
        vmPrn "[<"
        vmType "Import"
        vmPrn "("
        vmText $"\"{name}\", "
        vmKeyword "from"
        vmText $"=\"{source}\""
        vmPrn ")>]"
    ]


/// Gateway to interpret structures. Optionally adds Import attribute
let private interpretStructure (structure: StructureStatement) (tabLevel: TabLevel) =
    let tabbedImportAttribute name jsModule tabLevel =
        [
            tab tabLevel
            yield! importAttribute name jsModule
            vmEndLineNull
        ]

    interpreter {

        let! (
            config: {|
                ImportingJsModule: string; 
                FsStatementReader: FsStatementReader; 
                Interpreters: Interpreters
            |},
            tabLevel
        ) = Interpreter.ask

        match structure with
        | FunctionDefinition (FunctionDefinition.Plain (identifier, parameters, returnType)) ->
            let! fnInterpretation = 
                interpretFn "let" identifier parameters returnType 
                |> Interpreter.withEnv (fun (cfg, tabLevel) -> cfg.FsStatementReader, tabLevel)

            let display =
                [
                    yield! tabbedImportAttribute identifier config.ImportingJsModule tabLevel
                    tab tabLevel
                    yield! fnInterpretation
                    vmEndLineNull
                    vmEndLineNull
                ]

            let! signature = 
                interpretFnType parameters returnType 
                |> Interpreter.withEnv (fun (cfg, tabLevel) -> cfg.FsStatementReader, tabLevel)

            return FsStatement.Let (identifier, display, signature)

        | InterfaceDefinition (InterfaceDefinition.Plain (identifier, fl)) ->
            let! (display, body) = 
                config.Interpreters.InterpretPlainFableInterface identifier fl tabLevel
                |> Interpreter.withEnv (fun (config, tabLevel) -> config.FsStatementReader, tabLevel)

            return FsStatement.Typed (identifier, display, body)

        | ConstDefinition (DeclareConst (identifier, tdef)) ->
            let! typeInterpretation = 
                interpretTypeDefinition tdef
                |> Interpreter.withEnv (fun (config, tabLevel) -> config.FsStatementReader, tabLevel)

            match typeInterpretation with
            | Choice1Of2 l -> return FsStatement.Named (identifier, l)
            | Choice2Of2 vm -> return FsStatement.Link (identifier, vm)

        | TypeAlias (TypeAlias.Plain (identifier, Composition comb)) ->
            let body =
                comb 
                |> List.map (fun dtsType -> config.FsStatementReader)

            let display =
                [
                    tab tabLevel
                    vmKeyword "type "
                    vmIdentifier identifier
                    vmPrn " ="
                    vmEndLineNull
                ]

            return FsStatement.Typed (identifier, display, [])

        | _ -> 
            return
                [ 
                    vmText ($"{structure} interpretation is not implemented")
                    vmEndLineNull
                ]
                |> FsStatement.Nameless
    }


let internal toDocumentSegmentViewModelList (fsList: FsStatement list) : CodeItem list =
    fsList
    |> List.map FsStatement.codeItems
    |> List.concat


let inline logDebug category formatMessage formatParameters =
    interpreter {
        let! (loggerFactory: ILoggerFactory, _) = Interpreter.ask
        let logger = loggerFactory.CreateLogger(category)
        logger.LogDebug(formatMessage, formatParameters)
    }
    |> Interpreter.withEnv (fun (config, tabLevel) -> (^a: (member LoggerFactory: ILoggerFactory) config), tabLevel)


let inline logInfo category message =
    interpreter {
        let! (loggerFactory: ILoggerFactory, _) = Interpreter.ask
        let logger = loggerFactory.CreateLogger(category)
        logger.LogInformation(message)
    }
    |> Interpreter.withEnv (fun (config, tabLevel) -> (^a: (member LoggerFactory: ILoggerFactory) config), tabLevel)


let inline private storeFsStatement fsStatement =
    let category = "FsInterpreter.Facade.storeFsStatement"

    interpreter {
        let! (
            config:
                {|
                    Store: FsStatementStore 
                    LoggerFactory: ILoggerFactory
                    ModulePath: ModulePath 
                |}, 
            tabLevel
        ) = Interpreter.ask

        match fsStatement |> FsStatement.name with
        | Some n -> 
            do! logDebug category "Storing structure {name}..." [|n|]
            config.Store.Add config.ModulePath fsStatement
        | None -> 
            do! logDebug category "There is no structure for storing." [||]
    }
    |> Interpreter.withEnv (fun (config, tabLevel) -> 
        ({| 
            Store = (^a: (member Store: FsStatementStore) config);
            LoggerFactory = (^a: (member LoggerFactory: ILoggerFactory) config) 
            ModulePath = (^a: (member ModulePath: ModulePath) config) 
        |}, tabLevel)
    )


type private InterpretConfig' =
    {|
        Store : FsStatementStore
        Interpreters : Interpreters
        FsStatementReader : FsStatementReader
        ModulePath: ModulePath
        ImportingJsModule: string
        LoggerFactory: ILoggerFactory
        Namespace: string option
        TryFindModule: ModulePath -> StatementList option
    |}


let rec private _interpret statements tabLevel ind (result: FsStatementDto list) =
    let category = "FsInterpreter.Facade._interpret"

    interpreter {
        let! (config: InterpretConfig', _) = Interpreter.ask
    
        /// append generated view models to the result and invokes interpret
        let continueInterpret tail vm =
            _interpret tail tabLevel (ind + 1) (vm :: result)

        match statements with
        | statement :: tail ->

            let createDto vm = 
                let fsCodeStyle =
                    match vm with
                    | FsStatement.Typed _ -> FsCodeStyle.Fable
                    | _ -> FsCodeStyle.Universal
                FsStatementDto.create (statement |> Some) ind fsCodeStyle vm

            match statement with
            //| Statement.Import (importingEntities, Relative dtsModule) ->
            //    if config.FsStatementReader.ImportedModules |> Map.values |> Seq.contains dtsModule |> not then
            //        match config.TryFindModule dtsModule with
            //        | Some statements' ->
            //            do! 
            //                interpret config.Namespace dtsModule statements' 
            //                |> Interpreter.withEnv (fun (c: InterpretConfig', tabLevel) ->
            //                    {
            //                        Store = c.Store
            //                        Interpreters = c.Interpreters
            //                        LoggerFactory = c.LoggerFactory
            //                        TryFindModule = c.TryFindModule
            //                    }, tabLevel
            //                )
            //                |> Interpreter.map ignore
            //        | None -> ()

            //    return! _interpret tail tabLevel ind result
                
            | Statement.Export (ExportStatement.Structure structure)
            | Statement.Structure structure ->
                //do! logDebug category "Interpreting structure:\n {structure}..." [|structure|]
                
                let! (fsStatement: FsStatement) = 
                    interpretStructure structure tabLevel
                    |> Interpreter.withEnv (fun (config: InterpretConfig', tabLevel) ->
                        {|
                            ImportingJsModule = config.ImportingJsModule
                            FsStatementReader = config.FsStatementReader
                            Interpreters = config.Interpreters
                        |}, tabLevel
                    )
                            
               // do! storeFsStatement fsStatement

                return! continueInterpret tail (fsStatement |> createDto)

            | Statement.Export (ExportStatement.OutDefault identifier) ->
                let vm = 
                    match config.FsStatementReader.Get [identifier] with
                    | Some fsStatement -> createDto fsStatement
                    | None -> 
                        FsStatement.Nameless [
                            vmText $"%A{statement} is not parsed"
                            vmEndLineNull
                        ] |> createDto

                return! continueInterpret tail vm

            | _ -> return! _interpret tail tabLevel ind result

        | [] -> return result |> List.rev
    }
    


let fsNamespace (LibLocation libLocation) =
    let dir = Path.GetFileName(libLocation)
    String.Join(
        "",
        dir.Split([|' '; '-'|])
            |> Array.map (fun s -> Char.ToUpper(s[0]).ToString() + s[1..])
    )

let fsModuleName (ModulePath modulePath) =
    let dir = 
        match Path.GetFileName(modulePath) with
        | s when s.EndsWith(".d.ts") -> s[..^5]
        | s -> s

    String.Join(
        "",
        dir.Split([|' '; '-'; '.'|])
            |> Array.where (fun s -> Seq.length s > 1)
            |> Array.map (fun s -> Char.ToUpper(s[0]).ToString() + s[1..])
    )

let internal interpret libLocation modulePath statements : Interpreter<InterpretConfig, FsStatementDto list> =

    interpreter {
        let fileName = Path.GetFileNameWithoutExtension( modulePath |> ModulePath.Value )
        let jsModuleName = String( fileName |> Seq.takeWhile ((=) '.' >> not) |> Seq.toArray )

        let fsModuleName =
            let name =
                jsModuleName.Split('-')
                |> Seq.map (fun n ->
                    String( (Char.ToUpper(n[0]) :: (n |> Seq.skip 1 |> Seq.toList)) |> List.toArray)
                )
                |> fun l -> String.Join("", l |> Seq.toArray)

            match libLocation with
            | Some ns' -> $"{ns'}.{name}"
            | _ -> name

        let initialResult =
            (
                Identifier fsModuleName,
                [
                    vmKeyword "module "
                    vmText fsModuleName
                    vmEndLine null
                    vmEndLine null
                ] 
            )
            |> FsStatement.Named
            |> FsStatementDto.create None -1 FsCodeStyle.Universal
            |> List.singleton

        return! 
            _interpret statements (TabLevel 0) 0 initialResult
            |> Interpreter.withEnv (fun (config, tabLevel) -> 
                {| 
                    config with 
                        FsStatementReader = config.Store.InitReader modulePath
                        ModulePath = modulePath
                        ImportingJsModule = jsModuleName
                        Namespace = libLocation
                |}, tabLevel)
    }


/// <summary>
/// 
/// </summary>
/// <param name="rootFullPath"> Path to root folder. </param>
/// <param name="moduleFullPath"></param>
/// <param name="statement"></param>
/// <param name="interpretConfig"></param>
let rec internal toFsStatement rootFullPath moduleFullPath statement interpretConfig : Ports<InterpretConfigV2, (FsStatementV2 option * InnerInterpretConfig option)> =
    let storeFsStatment (fsStatement: FsStatementV2) =
        ports {
            let! config = Ports.ask
            let fsResult = 
                config.FsStatementStore.TryGetStatementList moduleFullPath
                |> Option.map (fun r ->
                    r 
                    |> Result.map (fun l -> l @ [fsStatement])
                    |> Result.orElseWith (fun _ -> Ok [fsStatement])
                )
                |> Option.defaultWith (fun () -> Ok [fsStatement])
            do
                config.FsStatementStore.AddOrUpdate moduleFullPath fsResult |> ignore
        }

    let interpretFsStatement innerConfig interpreter =
        ports {
            
            let fsStatement =
                interpreter
                |> Interpreter.run (innerConfig, TabLevel 0)

            do!
                storeFsStatment fsStatement

            return
                fsStatement |> Some
                , innerConfig |> Some
        }

    ports {
        let! config = Ports.ask
        let {InterpretStrategy = strategy; StatementStore = store; FsStatementStore = fsStore} = config

        let innerConfig : InnerInterpretConfig = 
            interpretConfig
            |> Option.defaultValue
                {
                    LibRelativePath =
                        lazy (
                            let rootPath = rootFullPath |> FullPath.Value
                            let modulePath = moduleFullPath |> FullPath.Value
                            Path.Combine(Path.GetFileName(rootPath), Path.GetRelativePath(rootPath, modulePath)[..^5])
                        )

                    TryGetLocal =
                        fun identifier ->
                            fsStore.TryGetStatement moduleFullPath identifier

                    TryGetStatement =
                        fun identifierList -> None
                }

        
        match statement with
        | Statement.Import (_, DtsModule.NodeModule _ ) ->
            return
                ( FsStatementV2.comment $"// outer lib is not processed yet - %O{statement}" |> Some, 
                  innerConfig |> Some )

        | Statement.Import (importEntityList, DtsModule.Relative (ModulePath relativePath) ) ->
            (*
                1а. Модуль разобран
                1b. Модуль не разобран
                    1. Разобрать модуль
            *)

            let tryGetLocal modulePath =
                fun identifier ->
                    let rec running importEntityList =
                        match importEntityList with
                        | head :: tail ->
                            match head with
                            | ImportEntity.Named identifier' when identifier = identifier' ->
                                fsStore.TryGetStatement modulePath identifier
                            | ImportEntity.Aliased (identifier', alias') when identifier = alias' ->
                                fsStore.TryGetStatement modulePath identifier
                            | _ -> running tail
                        | _ -> None

                    running importEntityList

            return
                None, // there is no fs statement
                Path.GetFullPath(
                    Path.Combine(
                        Path.GetDirectoryName(moduleFullPath |> FullPath.Value),
                        relativePath + ".d.ts"))
                |> FullPath.CreateOption
                |> Option.bind (fun modulePath' ->
                    if not (fsStore.ContainsKey modulePath') then
                        match store.TryGetStatementList modulePath' with
                        | Some (Result.Ok xs) -> 
                            interpretV2 rootFullPath modulePath' xs
                            |> Ports.run config
                            |> ignore
                            Some modulePath'
                        | _ -> None
                    else
                        Some modulePath'
                )
                |> Option.map (fun modulePath' ->
                    {innerConfig with
                        TryGetLocal =
                            fun identifier ->
                                tryGetLocal modulePath' identifier
                                |> Option.orElseWith (fun () -> innerConfig.TryGetLocal identifier)
                    }
                )
                |> Option.orElse interpretConfig

        | Statement.Export (ExportStatement.Structure (StructureStatement.InterfaceDefinition interfaceDefinition)) ->
            return! interpretFsStatement innerConfig (strategy.InterpretInterface interfaceDefinition)

        | Statement.Export (ExportStatement.Structure (StructureStatement.TypeAlias typeAlias)) ->
            return! interpretFsStatement innerConfig (strategy.InterpretTypeAlias typeAlias)

        | Statement.Export 
            (ExportStatement.StructureDefault 
                (StructureStatement.ClassDefinition (ClassDefinition.ExtendsEmpty (identifier, dtsType))))
                    when dtsType.ToString().StartsWith("React.Component") ->

            return! interpretFsStatement innerConfig (strategy.InterpretReactComponent identifier)

        | Statement.Comment comment ->
            return
                ( FsStatementV2.comment comment |> Some,
                  innerConfig |> Some )
        | _ ->
            let statementString = $"{statement}".Replace(Environment.NewLine, "")
            return
                ( FsStatementV2.comment $"(*\n {statementString} is not processed\n*)" |> Some,
                  innerConfig |> Some )
    }




let internal interpretV2 rootFullPath moduleFullPath statementList : Ports<InterpretConfigV2, FsStatementV2 list> =
    
    let rec running statementList reactInterpretConfig res =
        ports {
            match statementList with
            | [] -> return res |> List.choose id |> List.rev
            | statement :: tail ->
                let! (fsStatement, conf) = toFsStatement rootFullPath moduleFullPath statement reactInterpretConfig
                return! running tail conf (fsStatement::res)
        }

    ports {
        return! running statementList None []
    }


let internal appendNamespaceAndModules rootFullPath moduleFullPath fsStatements =
    let (namespaceStatements, moduleStatements) =
        fsStatements
        |> List.foldBack (fun s state ->
            match s.Scope with
            | Scope.Module moduleName -> (fst state, (moduleName, s) :: snd state)
            | _ -> (s :: fst state, snd state)
        ) <| ([], [])
        |> (fun (ns, ms) ->
            ns
            ,
            (ms 
            |> List.groupBy (fun (t: (string * FsStatementV2)) -> fst t)
            |> List.map (fun (key, t) -> (key, t |> List.map snd))
            |> List.map (fun (moduleName, xs) ->
                {
                    Identifier = FsStatmentKind.Module moduleName
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        vmKeyword "module "
                        vmType moduleName
                        vmPrn " ="
                        vmEndLineNull
                        vmEndLineNull
                        yield! (xs |> FsStatementV2.openCodeItems <| (ns |> FsStatementV2.opens)) |> CodeItem.increaseTab
                    ]
                    NestedStatements = []
                    PostCodeItems = []
                    Summary = []
                } :: (xs |> List.map FsStatementV2.increaseTab)
            )
            |> List.concat )
        )

    let namespaceName =
        let rootPath = rootFullPath |> FullPath.Value
        let modulePath = moduleFullPath |> FullPath.Value
        String.Join('.',
            seq {
                Path.GetFileName(rootPath)
                yield! 
                    Path.GetRelativePath(rootPath, modulePath)
                    .Split(Path.DirectorySeparatorChar)[..^1]
                Path.GetFileName(modulePath)[..^5]
            }
            |> Seq.map (fun s ->
                String.Join("",
                    s.Split("-") |> Seq.map FableTranspiler.Helpers.capitalizeFirstLetter
                )
            )
        )

    {
        Identifier = FsStatmentKind.Namespace namespaceName
        Scope = Scope.Inherit
        Open = []
        CodeItems = [
            vmKeyword "namespace "
            vmText namespaceName
            vmEndLineNull
            vmEndLineNull
            yield! namespaceStatements |> FsStatementV2.openCodeItems <| []
        ]
        NestedStatements = []
        PostCodeItems = []
        Summary = []
    } :: (namespaceStatements @ moduleStatements)
    
