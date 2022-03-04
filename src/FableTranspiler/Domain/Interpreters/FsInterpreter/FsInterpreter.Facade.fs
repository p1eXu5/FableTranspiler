module rec FableTranspiler.Interpreters.FsInterpreter.Facade

open System
open System.IO
open FsToolkit.ErrorHandling
open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types
open FableTranspiler.Ports.PortsBuilder
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter


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

                    FieldStartWithCodeItems = Fable.unionCase
                    InterfacePostCodeItems = Fable.inheritIHTMLProps
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

        | Statement.Export 
            (ExportStatement.Structure (StructureStatement.InterfaceDefinition interfaceDefinition))
                when (statement |> Statement.identifier |> Option.map Identifier.Value |> Option.defaultValue "").EndsWith("Props") ->
            
            return! 
                interpretFsStatement innerConfig (strategy.InterpretInterface interfaceDefinition |> Fable.withUnion)
                

        | Statement.Export 
            (ExportStatement.Structure (StructureStatement.InterfaceDefinition interfaceDefinition)) ->

            return! interpretFsStatement innerConfig (Fable.interpretInterface interfaceDefinition |> Fable.withAbstractClass)

        | Statement.Export (ExportStatement.Structure (StructureStatement.TypeAlias typeAlias)) ->
            return! interpretFsStatement innerConfig (strategy.InterpretTypeAlias typeAlias |> Fable.withUnion)

        | Statement.Export 
            (ExportStatement.StructureDefault 
                (StructureStatement.ClassDefinition (ClassDefinition.ExtendsEmpty (identifier, dtsType))))
                    when dtsType.ToString().StartsWith("React.Component") ->

            return! interpretFsStatement innerConfig (strategy.InterpretReactComponent identifier |> Fable.withUnion)

        | Statement.Structure (StructureStatement.ConstDefinition constDefinition) ->
            return! interpretFsStatement innerConfig (strategy.InterpretConstDefinition constDefinition |> Fable.withUnion)

        | Statement.NamespaceDeclaration (identifier, statementList) ->
            let! fsStatements = interpretV2 rootFullPath moduleFullPath statementList

            return
                ( None,
                  innerConfig |> Some )

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
                    Hidden = false
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
        Hidden = false
    } :: (namespaceStatements @ moduleStatements)
    
