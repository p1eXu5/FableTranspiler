namespace FableTranspiler.Adapters.WpfClient.Components

open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.DtsInterpreter
open FableTranspiler.Ports.Persistence
open FableTranspiler.Ports.PortsBuilder

[<ReferenceEquality>]
type DtsModule =
    {
        DtsStatements: Result<DtsStatement list, CodeItem list>
        SelectedDtsStatement: int option
    }

module internal DtsModule =
    
    type Msg = 
        | Interpret of FullPath


    open Elmish

    let init () =
        {
            DtsStatements = Result.Error []
            SelectedDtsStatement = None
        },
        Cmd.none


    let update msg model =
        ports {
            match msg with
            | Interpret fullPath ->
                let! (store: StatementStore) = Ports.ask
                match store.TryGetStatementList fullPath with
                | Some result ->
                    return
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
                    return
                        {   
                            model with
                                DtsStatements = Result.Error (CodeItem.interpretError $"Could not find {fullPath} module statements")
                                SelectedDtsStatement = None
                        },
                        Cmd.none
        }


    open Elmish.WPF

    let bindings () = [
        "SelectedDtsStatements" 
        |> Binding.oneWayOpt (fun m -> 
            match m.DtsStatements with
            | Ok statements -> statements |> Some
            | _ -> None
        )

        "SelectedDtsStatementsError" 
        |> Binding.oneWayOpt(fun m -> 
            match m.DtsStatements with
            | Error err -> err |> Some
            | _ -> None
        )
    ]