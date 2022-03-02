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

type FsStatementType =
    | No
    | Primitive
    | Array of FsStatementType
    | Func
    | Unit
    | FieldList of Qualifiers: Identifier list
    | Union
    | Composition
    | Unknown of string

type FsStatmentKind =
    | Comment
    | Type of FsStatementType
    | Interface of Identifier
    | Field of Identifier
    | Parameter of Identifier



type FsStatementV2 =
    {
        Identifier : FsStatmentKind
        Scope: Scope
        Open: string list
        CodeItems: CodeItem list
        NestedStatements: FsStatementV2 list
        Summary: CodeItem list
    }
    with
        override this.ToString() =
            let sb = StringBuilder()
            
            this.Summary
            |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)

            this.CodeItems
            |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)
            
            this.NestedStatements
            |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)

            sb.ToString()


type internal InnerInterpretConfig =
    {
        Namespace: Lazy<Scope>
        TryGetLocal: Identifier -> FsStatementV2 option
        TryGetStatement: Identifier list -> FsStatementV2 option
    }


type internal InterpretStrategy =
    {
        InterpretInterface: InterfaceDefinition -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretTypeAlias: TypeAlias -> Interpreter<InnerInterpretConfig, FsStatementV2>
    }

type internal InterpretConfigV2 =
    {
        InterpretStrategy: InterpretStrategy
        StatementStore: FableTranspiler.Ports.Persistence.StatementStore< Statement >
        FsStatementStore: FableTranspiler.Ports.Persistence.StatementStore< FsStatementV2 >
    }


module internal FsStatementV2 =

    let identifier statement =
        match statement.Identifier with
        | FsStatmentKind.Interface id -> id |> Some
        | _ -> None

    let zeroType =
        {
            Identifier = FsStatmentKind.Type FsStatementType.No
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = []
            Summary = []
        }

    let unitType =
        {
            Identifier = FsStatmentKind.Type FsStatementType.Unit
            Scope = Inherit
            Open = []
            CodeItems = [vmType "unit"]
            NestedStatements = []
            Summary = []
        }

    let arrayType =
        {
            Identifier = FsStatmentKind.Type FsStatementType.No
            Scope = Inherit
            Open = []
            CodeItems = [vmPrn " []"]
            NestedStatements = []
            Summary = []
        }

    let empty =
        {
            Identifier = FsStatmentKind.Comment
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = []
            Summary = []
        }

    let objType =
        {
            Identifier = FsStatmentKind.Type FsStatementType.Primitive
            Scope = Inherit
            Open = []
            CodeItems = [vmType "obj"]
            NestedStatements = []
            Summary = []
        }

    let comment message =
        {
            Identifier = FsStatmentKind.Comment
            Scope = Scope.Inherit
            Open = []
            CodeItems = [
                vmComment message
                vmEndLineNull
            ]
            NestedStatements = []
            Summary = [] 
        }

    let notZeroType = (<>) zeroType

    let toArray statement =
        match statement.Identifier with
        | FsStatmentKind.Type t ->
            {statement with 
                Identifier = FsStatmentKind.Type (FsStatementType.Array t)
                NestedStatements = statement.NestedStatements @ [arrayType]}
        | _ -> statement


    let rec codeItems statement =
        statement.Summary @ statement.CodeItems @ (statement.NestedStatements |> List.map (fun ns -> ns |> codeItems) |> List.concat)

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
        //| {Identifier = FsStatmentKind.Type }, {Identifier = FsStatmentKind.Type} ->
        //    {
        //        Identifier = FsStatmentKind.Type
        //        Scope = Inherit
        //        Open = statementA.Open @ statementB.Open
        //        CodeItems = statementA.CodeItems @ space @ statementB.CodeItems
        //        NestedStatements = statementA.NestedStatements @ statementB.NestedStatements
        //    }
        | _ -> failwith "Not implemented"
        

type FsStatementV2 with
    static member (+) (statementA: FsStatementV2, statementB: FsStatementV2) =
        FsStatementV2.add statementA statementB
    static member CollectCodeItems(fsStatement) = FsStatementV2.codeItems fsStatement
        
    