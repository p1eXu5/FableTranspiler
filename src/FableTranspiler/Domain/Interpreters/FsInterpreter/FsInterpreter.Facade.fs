module internal rec FableTranspiler.Interpreters.FsInterpreter.Facade

open System
open System.IO
open FsToolkit.ErrorHandling
open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types
open FableTranspiler.Ports.PortsBuilder
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler
open FableTranspiler.Interpreters.FsInterpreter

/// Return relative path to module for Import attributes
let libRelativePath rootFullPath moduleFullPath =
    let rootPath = rootFullPath |> FullPath.Value
    let modulePath = moduleFullPath |> FullPath.Value
    Path.Combine(Path.GetFileName(rootPath), Path.GetRelativePath(rootPath, modulePath)[..^5])


let defaultInnerInterpretConfig 
    (fsStore: Ports.Persistence.StatementStore<TopLevelFsStatement>) 
    rootFullPath 
    moduleFullPath 
        =
    {
        LibRelativePath =
            lazy (
                libRelativePath rootFullPath moduleFullPath
            )

        TryGetLocal =
            fun identifier ->
                fsStore.TryGetStatement moduleFullPath identifier

        TryGetStatement =
            fun identifierList -> None

        FieldStartWithCodeItems = Fable.unionCase
        InterfacePostCodeItems = Fable.inheritIHTMLProps
        InterfaceStatementKind = FsStatementKind.DU
        TypeScope = Scope.Namespace
        FuncParameterMapper = Fable.namedFuncSignature
        IsTypeSearchEnabled = true
        WrapFuncWithPrn = false
    }


/// <summary>
/// 
/// </summary>
/// <param name="rootFullPath"> Path to root folder. </param>
/// <param name="moduleFullPath"></param>
/// <param name="statement"></param>
/// <param name="interpretConfig"></param>
let rec internal toFsStatement rootFullPath moduleFullPath statement interpretConfig 
    : Ports<InterpretConfigV2, (Choice<TopLevelFsStatement, TopLevelFsStatement list, unit> * InnerInterpretConfig option)> =
    
    let storeFsStatment (fsStatement: TopLevelFsStatement) =
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

            if fsStatement.Kind = FsStatementKind.Container then
                let topLevelStatements = fsStatement.NestedStatements |> List.choose FsStatementV2.topLevel
                do
                    for s in topLevelStatements
                        do
                            storeFsStatment s |> ignore

                return
                    topLevelStatements |> Choice2Of3
                    , innerConfig |> Some
            else 
                do!
                    storeFsStatment fsStatement

                return
                    fsStatement |> Choice1Of3
                    , innerConfig |> Some
        }

    let interpretFsStatementOption innerConfig interpreter =
        ports {
            
            match 
                 interpreter
                 |> Interpreter.run (innerConfig, TabLevel 0) 
            with
            | Some fsStatement ->
                do!
                    storeFsStatment fsStatement

                return
                    fsStatement |> Choice1Of3
                    , innerConfig |> Some
            | None -> return Choice3Of3 (), innerConfig |> Some
        }

    let interpretFsStatementList innerConfig interpreter =
        ports {
            
            let fsStatements =
                interpreter
                |> Interpreter.run (innerConfig, TabLevel 0)

            do
                for s in fsStatements
                    do
                        storeFsStatment s |> ignore

            return
                fsStatements |> Choice2Of3
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
                            libRelativePath rootFullPath moduleFullPath
                        )

                    TryGetLocal =
                        fun identifier ->
                            fsStore.TryGetStatement moduleFullPath identifier

                    TryGetStatement =
                        fun identifierList -> None

                    FieldStartWithCodeItems = Fable.unionCase
                    InterfacePostCodeItems = Fable.inheritIHTMLProps
                    InterfaceStatementKind = FsStatementKind.DU
                    TypeScope = Scope.Namespace
                    FuncParameterMapper = Fable.namedFuncSignature
                    IsTypeSearchEnabled = true
                    WrapFuncWithPrn = false
                }

        
        match statement with
        | Statement.Import (_, DtsModule.NodeModule _ ) ->
            return
                ( FsStatementV2.comment $"// outer lib is not processed yet - %O{statement}" |> Choice1Of3, 
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
                Choice3Of3 (), // there is no fs statement
                Path.GetFullPath(
                    Path.Combine(
                        Path.GetDirectoryName(moduleFullPath |> FullPath.Value),
                        relativePath + ".d.ts"))
                |> FullPath.CreateOption
                |> Option.bind (fun modulePath' ->
                    if not (fsStore.ContainsKey modulePath') then
                        match store.TryGetStatementList modulePath' with
                        | Some (Result.Ok xs) -> 
                            interpretV2 rootFullPath modulePath' xs None
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

        // ================================
        //            Interface
        // ================================

        | Statement.Export
            (ExportStatement.Structure (StructureStatement.InterfaceDefinition interfaceDefinition))
                when (statement |> Statement.identifier |> Option.map Identifier.value |> Option.defaultValue "").EndsWith("Props") ->
            
            return! 
                interpretFsStatement 
                    innerConfig (strategy.InterpretInterface interfaceDefinition |> Fable.withUnion)
                
        | Statement.Structure (StructureStatement.InterfaceDefinition interfaceDefinition)
        | Statement.Export
            (ExportStatement.Structure (StructureStatement.InterfaceDefinition interfaceDefinition)) ->

            return! interpretFsStatement innerConfig (Fable.interpretInterface interfaceDefinition |> Fable.withAbstractClass)

        // ================================
        //            TypeAlias
        // ================================

        | Statement.Export (ExportStatement.Structure (StructureStatement.TypeAlias typeAlias)) ->
            return! interpretFsStatement innerConfig (strategy.InterpretTypeAlias typeAlias |> Fable.withUnion)

        // ================================
        //            React.Component
        // ================================

        | Statement.Export
            (ExportStatement.StructureDefault 
                (StructureStatement.ClassDefinition (ClassDefinition.ExtendsEmpty (identifier, dtsType))))
                    when dtsType.ToString().StartsWith("React.Component") ->

            return! interpretFsStatement innerConfig (strategy.InterpretReactComponent identifier |> Fable.withUnion)

        // ================================
        //            Const
        // ================================

        | Statement.Structure (StructureStatement.ConstDefinition constDefinition) ->
            return! interpretFsStatement innerConfig (strategy.InterpretConstDefinition constDefinition |> Fable.withUnion)

        // ================================
        //        FunctionDefinition
        // ================================

        | Statement.Structure (StructureStatement.FunctionDefinition functionDefinition)
        | Statement.Export (ExportStatement.StructureDefault (StructureStatement.FunctionDefinition functionDefinition))
        | Statement.Export (ExportStatement.Structure (StructureStatement.FunctionDefinition functionDefinition)) ->
            return! interpretFsStatement innerConfig (strategy.InterpretFunctionDefinition functionDefinition)

        // ================================
        //            Namespace
        // ================================

        | Statement.Export (ExportStatement.Namespace (identifier, statementList))
        | Statement.NamespaceDeclaration (identifier, statementList) ->
            let oldTypeScope = innerConfig.TypeScope
            let! namespaceFsStatements = interpretV2 rootFullPath moduleFullPath statementList ({innerConfig with TypeScope = Scope.Module ModuleScope.Main} |> Some)
            
            match! 
                interpretFsStatementOption 
                    {innerConfig with TypeScope = oldTypeScope} 
                    (strategy.InterpretNamespace identifier namespaceFsStatements |> Fable.withAbstractClass) 
            with
            | Choice1Of3 s, c -> return namespaceFsStatements @ [s] |> Choice2Of3, c
            | Choice2Of3 xs, c -> return namespaceFsStatements @ xs |> Choice2Of3, c
            | Choice3Of3 _, c -> return namespaceFsStatements |> Choice2Of3, c


        | Statement.Export (ExportStatement.OutDefault identifier) ->
            return
                innerConfig.TryGetLocal identifier
                |> Option.filter (FsStatementV2.isInterface)
                |> Option.map (fun _ ->
                    let identifier' = identifier |> Identifier.map Helpers.uncapitalizeFirstLetter

                    {
                        Kind = FsStatementKind.LetImportDefault identifier'
                        Scope = Scope.Module (ModuleScope.Main)
                        Open = ["Fable.Core"]
                        CodeItems = [
                            vmPrn "[<"; vmText "ImportDefault"; vmPrn "(@\""; vmPrn innerConfig.LibRelativePath.Value; vmPrn "\")>]"; vmEndLineNull
                            vmKeyword "let "; vmIdentifier identifier'; vmPrn " : "; vmTypeIdentifier identifier; vmPrn " = "; vmText "jsNative"; vmEndLineNull
                        ]
                        PostCodeItems = []
                        Hidden = false
                        Summary = []
                        NestedStatements = []
                    } |> Choice1Of3 
                    , innerConfig |> Some
                )
                |> Option.defaultWith (fun () ->
                    let statementString = $"{statement}".Replace(Environment.NewLine, "")
                    ( FsStatementV2.comment $"(*\n {statementString} is not processed\n*)" |> Choice1Of3,
                      innerConfig |> Some )
                )

        | Statement.Comment comment ->
            return
                ( FsStatementV2.comment comment |> Choice1Of3,
                  innerConfig |> Some )
        | _ ->
            let statementString = $"{statement}".Replace(Environment.NewLine, "")
            return
                ( FsStatementV2.comment $"(*\n {statementString} is not processed\n*)" |> Choice1Of3,
                  innerConfig |> Some )
    }


let interpretV2 rootFullPath moduleFullPath statementList innerConfig : Ports<InterpretConfigV2, TopLevelFsStatement list> =
    
    let rec running statementList innerConfig res =
        ports {
            match statementList with
            | [] -> return res |> List.rev
            | statement :: tail ->
                let! (fsStatementChoice, conf) = toFsStatement rootFullPath moduleFullPath statement innerConfig
                match fsStatementChoice with
                | Choice1Of3 fsStatement ->
                        return! running tail conf (fsStatement::res)
                | Choice2Of3 fsStatementList ->
                    return! running tail conf ((fsStatementList |> List.rev)  @ res)
                | Choice3Of3 _ ->
                    return! running tail conf res
        }

    ports {
        return! running statementList innerConfig []
    }


let appendNamespaceAndModules rootFullPath moduleFullPath (fsStatements: TopLevelFsStatement list) =
    let (namespaceStatements, moduleStatements) =
        fsStatements
        |> List.foldBack (fun s state ->
            match s.Scope with
            | Scope.Module (ModuleScope.Nested moduleName) -> (fst state, (moduleName, s) :: snd state)
            | _ -> (s :: fst state, snd state)
        ) <| ([], [])
        |> (fun (ns, ms) ->
            ns
            ,
            (ms 
            |> List.groupBy (fun (t: (string * TopLevelFsStatement)) -> fst t)
            |> List.map (fun (key, t) -> (key, t |> List.map snd))
            |> List.map (fun (moduleName, xs) ->
                {
                    Kind = FsStatementKind.Module moduleName
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        vmKeyword "module "
                        vmType moduleName
                        vmPrn " ="
                        vmEndLineNull
                        vmEndLineNull
                        yield! (xs |> TopLevelFsStatement.openCodeItems <| (ns |> TopLevelFsStatement.opens)) |> CodeItem.increaseTab
                    ]
                    NestedStatements = []
                    PostCodeItems = []
                    Summary = []
                    Hidden = false
                } :: (xs |> List.map TopLevelFsStatement.increaseTab)
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

    let topModule = namespaceStatements |> List.exists (fun s -> s.Scope |> Scope.isMainModule)
    {
        Kind =
            if topModule then
                FsStatementKind.Module namespaceName
            else
                FsStatementKind.Namespace namespaceName
                
        Scope = Scope.Inherit
        Open = []
        CodeItems = [
            if topModule then
                vmKeyword "module "
            else 
                vmKeyword "namespace "
            vmText namespaceName
            vmEndLineNull
            vmEndLineNull
            yield! namespaceStatements |> TopLevelFsStatement.openCodeItems <| []
        ]
        NestedStatements = []
        PostCodeItems = []
        Summary = []
        Hidden = false
    } :: (namespaceStatements @ moduleStatements)
    

let wrapLetImportsWithType rootFullPath moduleFullPath fsStatementList =
    ports {
        let! (config : InterpretConfigV2) = Ports.ask

        let toAbstractMember (fsLetStatement: TopLevelFsStatement) =
            let identifier = fsLetStatement |> TopLevelFsStatement.identifier |> Option.get
            {
                Kind = FsStatementKind.Field identifier
                Scope = Inherit
                Open = []
                CodeItems = [
                    tab (TabLevel 1)
                    vmKeyword "abstract "
                    vmIdentifier identifier
                    vmPrn " : "
                ]
                NestedStatements = fsLetStatement.NestedStatements
                PostCodeItems = [vmEndLineNull]
                Summary = fsLetStatement.Summary
                Hidden = false
            }
            |> FsStatementV2.TopLevelFsStatement


        let fsType letStatements = 
            letStatements
            |> List.groupBy (fun letTopLevelStatement -> letTopLevelStatement.Scope)
            |> List.map (fun (scope, letTopLevelStatementList) ->
                let typeIdentifier =
                    match scope with
                    | Scope.Module (ModuleScope.Main) ->
                        String.Join("",
                            (Path.GetFileName(moduleFullPath |> FullPath.Value)[..^5]).Split("-")
                            |> Seq.map Helpers.capitalizeFirstLetter
                        )
                        |> Identifier.create
                    | _ -> failwith "Not implemented"

                let identifier' = typeIdentifier |> Identifier.map Helpers.uncapitalizeFirstLetter
                let libPath = libRelativePath rootFullPath moduleFullPath
                [
                    {
                        Kind = FsStatementKind.AbstractClass typeIdentifier
                        Scope = scope
                        Open = []
                        CodeItems = [
                            vmKeyword "type "; vmTypeIdentifier typeIdentifier; vmPrn " ="; vmEndLineNull
                        ]
                        NestedStatements = 
                            letTopLevelStatementList
                            |> List.map toAbstractMember 
                        PostCodeItems = []
                        Summary = []
                        Hidden = false
                    }
                    {
                        Kind = FsStatementKind.LetImportDefault identifier'
                        Scope = Scope.Module (ModuleScope.Main)
                        Open = ["Fable.Core"]
                        CodeItems = [
                            vmPrn "[<"; vmText "ImportDefault"; vmPrn "(@\""; vmPrn libPath; vmPrn "\")>]"; vmEndLineNull
                            vmKeyword "let "; vmIdentifier identifier'; vmPrn " : "; vmTypeIdentifier typeIdentifier; vmPrn " = "; vmText "jsNative"; vmEndLineNull
                        ]
                        PostCodeItems = []
                        Hidden = false
                        Summary = []
                        NestedStatements = []
                    }
                ]
            )
            |> List.concat


        let fsStatements' =
            fsStatementList
            |> List.partition FsStatementV2.isLetImport
            |> (fun (letStatements, elseStatements) ->
                elseStatements @ (fsType letStatements)
            )

        do
            config.FsStatementStore.AddOrUpdate moduleFullPath (Result.Ok fsStatements') |> ignore

        return fsStatements'
    }
