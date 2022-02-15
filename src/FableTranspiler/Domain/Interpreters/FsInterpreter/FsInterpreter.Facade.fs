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
            |}
        ) = Interpreter.ask

        match structure with
        | FunctionDefinition (FunctionDefinition.Plain (identifier, parameters, returnType)) ->
            let! fnInterpretation = 
                interpretFn "let" identifier parameters returnType 
                |> Interpreter.withEnv (fun cfg -> cfg.FsStatementReader)

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
                |> Interpreter.withEnv (fun cfg -> cfg.FsStatementReader)

            return FsStatement.Let (identifier, display, signature)

        | InterfaceDefinition (InterfaceDefinition.Plain (identifier, fl)) ->
            let! (display, body) = 
                config.Interpreters.InterpretPlainFableInterface identifier fl tabLevel
                |> Interpreter.withEnv (fun config -> config.FsStatementReader)

            return FsStatement.Typed (identifier, display, body)

        | ConstDefinition (DeclareConst (identifier, tdef)) ->
            let! typeInterpretation = 
                interpretTypeDefinition tdef
                |> Interpreter.withEnv (fun config -> config.FsStatementReader)

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
        let! (loggerFactory: ILoggerFactory) = Interpreter.ask
        let logger = loggerFactory.CreateLogger(category)
        logger.LogDebug(formatMessage, formatParameters)
    }
    |> Interpreter.withEnv (fun config -> (^a: (member LoggerFactory: ILoggerFactory) config))


let inline logInfo category message =
    interpreter {
        let! (loggerFactory: ILoggerFactory) = Interpreter.ask
        let logger = loggerFactory.CreateLogger(category)
        logger.LogInformation(message)
    }
    |> Interpreter.withEnv (fun config -> (^a: (member LoggerFactory: ILoggerFactory) config))


let inline private storeFsStatement fsStatement =
    let category = "FsInterpreter.Facade.storeFsStatement"

    interpreter {
        let! (
            config:
                {|
                    Store: FsStatementStore 
                    LoggerFactory: ILoggerFactory
                    ModulePath: ModulePath 
                |}
        ) = Interpreter.ask

        match fsStatement |> FsStatement.name with
        | Some n -> 
            do! logDebug category "Storing structure {name}..." [|n|]
            config.Store.Add config.ModulePath fsStatement
        | None -> 
            do! logDebug category "There is no structure for storing." [||]
    }
    |> Interpreter.withEnv (fun config -> 
        {| 
            Store = (^a: (member Store: FsStatementStore) config);
            LoggerFactory = (^a: (member LoggerFactory: ILoggerFactory) config) 
            ModulePath = (^a: (member ModulePath: ModulePath) config) 
        |}
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
        let! (config: InterpretConfig') = Interpreter.ask
    
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
            | Statement.Import (importingEntities, Relative dtsModule) ->
                if config.FsStatementReader.ImportedModules |> Map.values |> Seq.contains dtsModule |> not then
                    match config.TryFindModule dtsModule with
                    | Some statements' ->
                        do! 
                            interpret config.Namespace dtsModule statements' 
                            |> Interpreter.withEnv (fun (c: InterpretConfig') ->
                                {
                                    Store = c.Store
                                    Interpreters = c.Interpreters
                                    LoggerFactory = c.LoggerFactory
                                    TryFindModule = c.TryFindModule
                                }
                            )
                            |> Interpreter.map ignore
                    | None -> ()

                return! _interpret tail tabLevel ind result
                
            | Statement.Export (ExportStatement.Structure structure)
            | Statement.Structure structure ->
                do! logDebug category "Interpreting structure:\n {structure}..." [|structure|]
                
                let! (fsStatement: FsStatement) = 
                    interpretStructure structure tabLevel
                    |> Interpreter.withEnv (fun (config: InterpretConfig') ->
                        {|
                            ImportingJsModule = config.ImportingJsModule
                            FsStatementReader = config.FsStatementReader
                            Interpreters = config.Interpreters
                        |}
                    )
                            
                do! storeFsStatement fsStatement

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
            |> Interpreter.withEnv (fun config -> 
                {| 
                    config with 
                        FsStatementReader = config.Store.InitReader modulePath
                        ModulePath = modulePath
                        ImportingJsModule = jsModuleName
                        Namespace = libLocation
                |})
    }
