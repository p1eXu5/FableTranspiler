namespace FableTranspiler.Adapters.WpfClient.Components

open FableTranspiler.Interpreters
open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Interpreters.FsInterpreter.Facade
open FableTranspiler.Ports.PortsBuilder
open FableTranspiler.Ports.Persistence
open FableTranspiler.Adapters.Persistence
open FableTranspiler.SimpleTypes


[<ReferenceEquality>]
type internal FsModule =
    {
        FsStatements: Result<TopLevelFsStatement list, ErrorDescription>
        SelectedFsStatement: int option
        MutedFsStatements: int list
        InterpretConfigV2: InterpretConfigV2
        FileName: string option
    }


module internal FsModule =
    
    type Msg =
        | Interpret of RootFullPath: FullPath * ModuleFullPath: FullPath


    open Elmish

    let init statementStore =
        {
            FsStatements = Ok []
            SelectedFsStatement = None
            MutedFsStatements = []
            InterpretConfigV2 =
                {
                    InterpretStrategy = Fable.strategy
                    StatementStore = statementStore
                    FsStatementStore = StatementStore.create TopLevelFsStatement.identifier
                }
            FileName = None
        }
        , Cmd.none

    let update msg model =
        match msg with
        | Interpret (rootFullPath, moduleFullPath) ->
            let fsStatementResult =
                model.InterpretConfigV2.FsStatementStore.TryGetStatementList moduleFullPath
                |> Option.map (fun fsStatementsResult ->
                    fsStatementsResult 
                    |> Result.map (fun fsStatements -> fsStatements |> appendNamespaceAndModules rootFullPath moduleFullPath) 
                    |> Result.mapError (CodeItem.interpretError)
                )
                |> Option.defaultWith (fun () -> 
                    model.InterpretConfigV2.StatementStore.TryGetStatementList moduleFullPath
                    |> Option.map (fun dtsStatementsResult ->
                        dtsStatementsResult
                        |> Result.map (fun statements ->
                            interpretV2 rootFullPath moduleFullPath statements None
                            |> Ports.run model.InterpretConfigV2
                            |> appendNamespaceAndModules rootFullPath moduleFullPath
                        )
                        |> Result.map (List.filter FsStatementV2.notHidden)
                        |> Result.mapError (CodeItem.interpretError)
                    )
                    |> Option.defaultWith (fun () ->  Result.Error (CodeItem.interpretError $"Could not find {moduleFullPath} module statements"))
                )
        
            {
                model with
                    FsStatements = fsStatementResult
                    FileName =
                        match fsStatementResult with
                        | Ok xs when xs.Length > 0 ->
                            match xs.Head.Kind with
                            | FsStatementKind.Namespace n
                            | FsStatementKind.Module n ->
                                n.Split(".") |> Array.last |> (+) <| ".fs" |> Some
                            | _ -> None
                        | _ -> None

                    SelectedFsStatement = None
            },
            Cmd.none


    open Elmish.WPF

    let bindings () = [
        "FsStatements" 
        |> Binding.oneWayOpt (fun m -> 
            match m.FsStatements with
            | Ok statements -> statements |> Some
            | _ -> None
        )

        "FsStatementsError" 
        |> Binding.oneWayOpt(fun m -> 
            match m.FsStatements with
            | Error err -> err |> Some
            | _ -> None
        )

        "FileName" |> Binding.oneWayOpt (fun m -> m.FileName)

        "HasFsStatements" |> Binding.oneWay (fun m -> 
            match m.FsStatements with
            | Ok l when not (l |> List.isEmpty) -> true
            | _ -> false
        )
    ]