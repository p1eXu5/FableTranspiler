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
        FsStatements: Result<FsStatementV2 list, ErrorDescription>
        SelectedFsStatement: int option
        MutedFsStatements: int list
        InterpretConfigV2: InterpretConfigV2
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
                    InterpretStrategy = React.strategy
                    StatementStore = statementStore
                    FsStatementStore = StatementStore.create FsStatementV2.identifier
                }
        }
        , Cmd.none

    let update msg model =
            match msg with
            | Interpret (rootFullPath, moduleFullPath) ->
                match model.InterpretConfigV2.FsStatementStore.TryGetStatementList moduleFullPath with
                | Some result ->
                    {
                        model with
                            FsStatements = result |> Result.mapError (CodeItem.interpretError)
                            SelectedFsStatement = None
                    },
                    Cmd.none
                | None ->
                    match model.InterpretConfigV2.StatementStore.TryGetStatementList moduleFullPath with
                    | Some result ->
                        {
                            model with
                                FsStatements = 
                                    result 
                                    |> Result.map (fun statements ->
                                        interpretV2 rootFullPath moduleFullPath statements
                                        |> Ports.run model.InterpretConfigV2
                                    )
                                    |> Result.mapError (CodeItem.interpretError)
                                SelectedFsStatement = None
                        },
                        Cmd.none
                    | None ->
                        {   
                            model with
                                FsStatements = Result.Error (CodeItem.interpretError $"Could not find {moduleFullPath} module statements")
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
    ]