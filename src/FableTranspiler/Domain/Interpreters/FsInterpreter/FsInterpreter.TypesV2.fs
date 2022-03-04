namespace FableTranspiler.Interpreters.FsInterpreter

open System
open FableTranspiler.Parsers.Types
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters
open Microsoft.Extensions.Logging
open System.Text

type Summary = CodeItem list

type Scope =
    | Namespace
    | Inherit
    | Module of string

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
    | ReactComponent of Identifier
    | Object of Identifier
    | Const of Identifier
    | Namespace of string
    | Module of string



type FsStatementV2 =
    {
        Identifier : FsStatmentKind
        Scope: Scope
        Open: string list
        CodeItems: CodeItem list
        NestedStatements: FsStatementV2 list
        /// make sense only on root level
        PostCodeItems: CodeItem list
        Summary: CodeItem list
        Hidden: bool
    }
    with
        member private this.ToStringInner() =
            let sb = StringBuilder()
            if not this.Hidden then 
                this.Summary
                |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)

                this.CodeItems
                |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)
            
                this.NestedStatements
                |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)

            sb

        override this.ToString() =
            let sb = this.ToStringInner()

            this.PostCodeItems
            |> List.iter (fun s -> sb.Append(s.ToString()) |> ignore)

            sb.ToString()


type internal InnerInterpretConfig =
    {
        LibRelativePath: Lazy<string>
        TryGetLocal: Identifier -> FsStatementV2 option
        TryGetStatement: Identifier list -> FsStatementV2 option
        InterfacePostCodeItems: Interpreter<InnerInterpretConfig, CodeItem list>
        FieldStartWithCodeItems: Identifier -> Interpreter<InnerInterpretConfig, CodeItem list>
    }




type internal InterpretStrategy =
    {
        InterpretInterface: InterfaceDefinition -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretTypeAlias: TypeAlias -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretReactComponent: Identifier (* -> DtsType *) -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretConstDefinition: ConstDefinition -> Interpreter<InnerInterpretConfig, FsStatementV2>
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
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let unitType =
        {
            Identifier = FsStatmentKind.Type FsStatementType.Unit
            Scope = Inherit
            Open = []
            CodeItems = [vmType "unit"]
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let arrayType =
        {
            Identifier = FsStatmentKind.Type FsStatementType.No
            Scope = Inherit
            Open = []
            CodeItems = [vmPrn " []"]
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let empty =
        {
            Identifier = FsStatmentKind.Comment
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let objType =
        {
            Identifier = FsStatmentKind.Type FsStatementType.Primitive
            Scope = Inherit
            Open = []
            CodeItems = [vmType "obj"]
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
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
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let htmlPropsInheritance tabLevel =
        [
            tab tabLevel
            vmKeyword "interface "
            vmType "IHTMLProp"
            vmEndLineNull
        ]


    let rec increaseTab statement =
        {
            statement with
                CodeItems = CodeItem.increaseTab statement.CodeItems
                NestedStatements = statement.NestedStatements |> List.map increaseTab
                PostCodeItems = CodeItem.increaseTab statement.PostCodeItems
        }


    let notZeroType = (<>) zeroType

    let toArray statement =
        match statement.Identifier with
        | FsStatmentKind.Type t ->
            {statement with 
                Identifier = FsStatmentKind.Type (FsStatementType.Array t)
                NestedStatements = statement.NestedStatements @ [arrayType]}
        | _ -> statement


    let codeItems statement =
        let rec codeItems statement =
            statement.Summary @ statement.CodeItems @ (statement.NestedStatements |> List.map (fun ns -> ns |> codeItems) |> List.concat)

        codeItems statement @ statement.PostCodeItems


    let rec opens statements : string list =
        statements
        |> List.map (fun s -> s.Open @ (s.NestedStatements |> opens)) 
        |> List.concat
        |> List.distinct


    let rec openCodeItems statements (except: #seq<_>) =
        let rec opens statement =
            statement.Open @ (statement.NestedStatements |> List.map (fun n -> opens n) |> List.concat)

        statements
        |> List.map opens
        |> List.concat
        |> List.except except
        |> List.distinct
        |> List.map (fun o ->
            [
                tab (TabLevel 0)
                vmKeyword "open "
                vmText o
                vmEndLineNull
            ]
        )
        |> List.concat


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
        
    