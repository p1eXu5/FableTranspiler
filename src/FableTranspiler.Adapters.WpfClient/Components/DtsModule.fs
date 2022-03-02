namespace FableTranspiler.Adapters.WpfClient.Components

open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.DtsInterpreter
open FableTranspiler.Ports.Persistence
open FableTranspiler.Ports.PortsBuilder
open FableTranspiler.Parsers.Types

type ErrorDescription = CodeItem list

[<ReferenceEquality>]
type DtsModule =
    {
        StatementsStore: StatementStore<Statement>
        DtsStatements: Result<DtsStatement list, ErrorDescription>
        SelectedDtsStatement: int option
    }

module internal DtsModule =
    
    type Msg = 
        | Interpret of FullPath


    open Elmish

    let init store =
        {
            StatementsStore = store
            DtsStatements = Result.Ok []
            SelectedDtsStatement = None
        },
        Cmd.none


    let update msg model =
        match msg with
        | Interpret fullPath ->
            match model.StatementsStore.TryGetStatementList fullPath with
            | Some result ->
                {
                    model with
                        DtsStatements =
                            result
                            |> Result.map (fun statements ->
                                interpret statements
                            )
                            |> Result.mapError (CodeItem.interpretError)
                        SelectedDtsStatement = None
                },
                Cmd.none
            | None -> 
                {   
                    model with
                        DtsStatements = Result.Error (CodeItem.interpretError $"Could not find {fullPath} module statements")
                        SelectedDtsStatement = None
                },
                Cmd.none


    open Elmish.WPF

    let bindings () = [
        "DtsStatements" 
        |> Binding.oneWayOpt (fun m -> 
            match m.DtsStatements with
            | Ok statements -> statements |> Some
            | _ -> None
        )

        "DtsStatementsError" 
        |> Binding.oneWayOpt(fun m -> 
            match m.DtsStatements with
            | Error err -> err |> Some
            | _ -> None
        )
    ]