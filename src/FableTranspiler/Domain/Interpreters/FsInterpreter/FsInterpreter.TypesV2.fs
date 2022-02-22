namespace FableTranspiler.Interpreters.FsInterpreter

open System
open FableTranspiler.Parsers.Types
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters
open Microsoft.Extensions.Logging
open System.Text


type Scope =
    | Namespace of string
    | Inherit


type FsStatmentKind =
    | Type
    | Interface of Identifier
    | Field of Identifier
    | Parameter of Identifier
    | Union

type FsStatementV2 =
    {
        Identifier : FsStatmentKind
        Scope: Scope
        Open: string list
        CodeItems: CodeItem list
        NestedStatements: FsStatementV2 list
    }
    with
        override this.ToString() =
            let sb = StringBuilder()
            
            this.CodeItems
            |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)
            
            this.NestedStatements
            |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)

            sb.ToString()


module internal FsStatementV2 =

    let identifier statement =
        match statement.Identifier with
        | FsStatmentKind.Interface id -> id |> Some
        | _ -> None

    let zeroType =
        {
            Identifier = FsStatmentKind.Type
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = []
        }

    let unitType =
        {
            Identifier = FsStatmentKind.Type
            Scope = Inherit
            Open = []
            CodeItems = [vmType "unit"]
            NestedStatements = []
        }


    let notZeroType = (<>) zeroType


    let rec codeItems statement =
        statement.CodeItems @ (statement.NestedStatements |> List.map (fun ns -> ns |> codeItems) |> List.concat)

    let rec addLineBreak statement =
        match statement.NestedStatements with
        | [] ->
            {statement with CodeItems = statement.CodeItems @ [vmEndLineNull]}
        | _ ->
            {statement with NestedStatements = statement.NestedStatements[..^1] @ [(addLineBreak (statement.NestedStatements |> List.last))]}

    let add statementA statementB =
        let space = [vmText " "]

        match statementA, statementB with
        | a, b when a = zeroType && b = zeroType -> zeroType
        | _, b when b = zeroType -> statementA
        | a, _ when a = zeroType -> statementB
        | {Identifier = FsStatmentKind.Type}, {Identifier = FsStatmentKind.Type} ->
            {
                Identifier = FsStatmentKind.Type
                Scope = Inherit
                Open = statementA.Open @ statementB.Open
                CodeItems = statementA.CodeItems @ space @ statementB.CodeItems
                NestedStatements = statementA.NestedStatements @ statementB.NestedStatements
            }
        | _ -> failwith "Not implemented"
        

type FsStatementV2 with
    static member (+) (statementA: FsStatementV2, statementB: FsStatementV2) =
        FsStatementV2.add statementA statementB
    