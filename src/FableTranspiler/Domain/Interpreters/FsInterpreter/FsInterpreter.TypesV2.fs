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
    | Module of ModuleScope
and 
    ModuleScope =
        | Nested of string
        | Main


type FsStatementType =
    | No
    | Primitive
    | Array of FsStatementType
    | Func
    | Unit
    | FieldList of Qualifiers: Identifier list
    | Anonymous
    | Union
    | Composition
    | Unknown of string
    | Reference of string

type FsStatementKind =
    | Comment
    | Type of FsStatementType
    | DU of Identifier
    | AbstractClass of Identifier
    | Field of Identifier
    /// let statement with ImportDefaultAttribute
    | LetImportDefault of Identifier
    /// let statement with ImportAttribute
    | LetImport of Identifier
    | Let of Identifier
    | Parameter of Identifier
    | ReactComponent of Identifier
    | Object of Identifier
    | Const of Identifier
    | Namespace of string
    | Module of string
    | Container



type FsStatementV2 =
    {
        Kind : FsStatementKind
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
        FieldStartWithCodeItems: Interpreter<InnerInterpretConfig, Identifier -> CodeItem list>
        InterfaceStatementKind: Identifier -> FsStatementKind
        TypeScope: Scope
        InterpretFuncSignature: FieldList -> ReturnTypeDefinition -> FsStatementKind -> CodeItemPrependix -> CodeItemAppendix -> Interpreter<InnerInterpretConfig, (FsStatementV2 * Summary)>
        IsTypeSearchEnabled: bool
        WrapFuncWithPrn: bool
    }
and
    ReturnTypeDefinition = TypeDefinition
and
    CodeItemPrependix = CodeItem list
and
    CodeItemAppendix = CodeItem list


module internal InnerInterpretConfig = 
    
    let inline wrapFuncWithPrn<'a> (interpreter: Interpreter<InnerInterpretConfig, 'a>) : Interpreter<InnerInterpretConfig, 'a> =
       interpreter 
       |> Interpreter.withEnv (fun (c, tabLevel) ->
           { c with
                 WrapFuncWithPrn = true
           }
           , tabLevel
       )
    
    let inline unwrapFuncWithPrn<'a> (interpreter: Interpreter<InnerInterpretConfig, 'a>) : Interpreter<InnerInterpretConfig, 'a> =
       interpreter 
       |> Interpreter.withEnv (fun (c, tabLevel) ->
           { c with
                 WrapFuncWithPrn = false
           }
           , tabLevel
       )


type internal InterpretStrategy =
    {
        InterpretInterface : InterfaceDefinition -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretTypeAlias : TypeAlias -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretReactComponent : Identifier (* -> DtsType *) -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretConstDefinition : ConstDefinition -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretNamespace : Identifier -> FsStatementV2 list -> Interpreter<InnerInterpretConfig, FsStatementV2>
        InterpretFunctionDefinition : FunctionDefinition -> Interpreter<InnerInterpretConfig, FsStatementV2>
    }

type internal InterpretConfigV2 =
    {
        InterpretStrategy: InterpretStrategy
        StatementStore: FableTranspiler.Ports.Persistence.StatementStore< Statement >
        FsStatementStore: FableTranspiler.Ports.Persistence.StatementStore< FsStatementV2 >
    }


module internal FsStatementV2 =

    let identifier statement =
        match statement.Kind with
        | FsStatementKind.Const id
        | FsStatementKind.Object id
        | FsStatementKind.ReactComponent id
        | FsStatementKind.LetImportDefault id
        | FsStatementKind.LetImport id
        | FsStatementKind.Let id
        | FsStatementKind.Field id
        | FsStatementKind.Parameter id
        | FsStatementKind.AbstractClass id
        | FsStatementKind.DU id -> id |> Some
        | _ -> None

    let zeroType =
        {
            Kind = FsStatementKind.Type FsStatementType.No
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let primitiveType ``type`` =
        {
            Kind = FsStatementKind.Type FsStatementType.Primitive
            Scope = Inherit
            Open = []
            CodeItems = [vmType ``type``]
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let reference ``type`` =
        {
            Kind = FsStatementKind.Type (FsStatementType.Reference ``type``)
            Scope = Inherit
            Open = []
            CodeItems = [vmType ``type``]
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let unitType =
        {
            Kind = FsStatementKind.Type FsStatementType.Unit
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
            Kind = FsStatementKind.Type FsStatementType.No
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
            Kind = FsStatementKind.Comment
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
            Kind = FsStatementKind.Type FsStatementType.Primitive
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
            Kind = FsStatementKind.Comment
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
        match statement.Kind with
        | FsStatementKind.Type t ->
            {statement with 
                Kind = FsStatementKind.Type (FsStatementType.Array t)
                NestedStatements = statement.NestedStatements @ [arrayType]}
        | _ -> statement


    let rec codeItems statement =
        statement.Summary @ statement.CodeItems @ (statement.NestedStatements |> List.map (fun ns -> ns |> codeItems) |> List.concat) @ statement.PostCodeItems



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


    let isInterface s =
        match s.Kind with
        | FsStatementKind.DU _
        | FsStatementKind.AbstractClass _ -> true
        | _ -> false


    let notHidden s = not (s.Hidden)

    let isLet s =
        match s.Kind with
        | FsStatementKind.LetImport _ -> true
        | _ -> false

    let isAnonymousType s =
        match s.Kind with
        | FsStatementKind.Type FsStatementType.Anonymous -> true
        | _ -> false

    let isFieldListType s =
        match s.Kind with
        | FsStatementKind.Type (FsStatementType.FieldList _) -> true
        | _ -> false

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
        
    

module Scope =
    let isMainModule = function
        | Scope.Module (ModuleScope.Main) -> true
        | _ -> false