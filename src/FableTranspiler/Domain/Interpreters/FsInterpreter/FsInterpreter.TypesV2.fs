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
    | Parameter of Identifier
    | Empty
    | Operator
    | Primitive
    | Array of FsStatementType
    | FuncSignature
    | Unit
    | FieldList of Qualifiers: Identifier list
    | Field of Identifier
    | Anonymous
    | Union
    | Composition
    | Unknown of string
    | Reference of Identifier list
    | ReferenceGeneric of Identifier list
    | ReactElement
    | ReactElementType

type FsStatementKind =
    | Comment
    //| Type of FsStatementType
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
    | InnerFsStatement of InnerFsStatement
    | TopLevelFsStatement of TopLevelFsStatement
    with
        override this.ToString() =
            match this with
            | InnerFsStatement s -> s.ToString()
            | TopLevelFsStatement s -> s.ToString()

        static member identifier statement =
            match statement with
            | FsStatementV2.InnerFsStatement s -> s |> InnerFsStatement.identifier
            | FsStatementV2.TopLevelFsStatement s -> s |> TopLevelFsStatement.identifier |> Option.map List.singleton |> Option.defaultValue []


and
    InnerFsStatement =
    {
        Type : FsStatementType
        Open: string list
        CodeItems: CodeItem list
        NestedStatements: InnerFsStatement list
        /// make sense only on root level
        PostCodeItems: CodeItem list
    }
    with
        member private this.ToStringInner() =
            let sb = StringBuilder()
        
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

        static member identifier statement =
            match statement.Type with
            | Reference l
            | ReferenceGeneric l -> l
            | FsStatementType.Parameter id
            | FsStatementType.Field id -> [id]
            | _ -> []
and
    TopLevelFsStatement =
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

        static member identifier statement =
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






type internal InnerInterpretConfig =
    {
        LibRelativePath: Lazy<string>
        TryGetLocal: Identifier -> TopLevelFsStatement option
        TryGetStatement: Identifier list -> TopLevelFsStatement option
        InterfacePostCodeItems: Interpreter<InnerInterpretConfig, CodeItem list>
        FieldStartWithCodeItems: Interpreter<InnerInterpretConfig, Identifier -> CodeItem list>
        InterfaceStatementKind: Identifier -> FsStatementKind
        TypeScope: Scope
        IsTypeSearchEnabled: bool
        WrapFuncWithPrn: bool
        FuncParameterMapper: InnerFsStatement -> InnerFsStatement
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


    let inline withFuncSignature mapper (interpreter: Interpreter<InnerInterpretConfig, 'a>) : Interpreter<InnerInterpretConfig, 'a> =
        interpreter
        |> Interpreter.withEnv (fun (config, tabLevel) ->
            { config with FuncParameterMapper = mapper }, tabLevel
        )


    //let inline withFuncSignatureInterpreter interpretFuncSignature interpreter =
    //    interpreter
    //    |> Interpreter.withEnv (fun (config, tabLevel) ->
    //        { config with InterpretFuncSignature = interpretFuncSignature }, tabLevel
    //    )
    
    
    let inline withDisabledTypeSearching interpreter =
        interpreter
        |> Interpreter.withEnv (fun (config, tabLevel) ->
            { config with IsTypeSearchEnabled = false }, tabLevel
        )
    
    let withEnableTypeSearching interpreter =
        interpreter
        |> Interpreter.withEnv (fun (config, tabLevel) ->
            { config with IsTypeSearchEnabled = true }, tabLevel
        )


type internal InterpretStrategy =
    {
        InterpretInterface : InterfaceDefinition -> Interpreter<InnerInterpretConfig, TopLevelFsStatement>
        InterpretTypeAlias : TypeAlias -> Interpreter<InnerInterpretConfig, TopLevelFsStatement>
        InterpretReactComponent : Identifier (* -> DtsType *) -> Interpreter<InnerInterpretConfig, TopLevelFsStatement>
        InterpretConstDefinition : ConstDefinition -> Interpreter<InnerInterpretConfig, TopLevelFsStatement>
        InterpretNamespace : Identifier -> TopLevelFsStatement list -> Interpreter<InnerInterpretConfig, TopLevelFsStatement option>
        InterpretFunctionDefinition : FunctionDefinition -> Interpreter<InnerInterpretConfig, TopLevelFsStatement>
    }

type internal InterpretConfigV2 =
    {
        InterpretStrategy: InterpretStrategy
        StatementStore: FableTranspiler.Ports.Persistence.StatementStore< Statement >
        FsStatementStore: FableTranspiler.Ports.Persistence.StatementStore< TopLevelFsStatement >
    }


module internal InnerFsStatement =

    let zeroType =
        {
            Type = FsStatementType.Empty
            Open = []
            CodeItems = []
            NestedStatements = []
            PostCodeItems = []
        }

    let notZeroType = (<>) zeroType

    let primitiveType ``type`` =
        {
            Type = FsStatementType.Primitive
            Open = []
            CodeItems = [vmType ``type``]
            NestedStatements = []
            PostCodeItems = []
        }

    let reference ``type`` =
        {
            Type = FsStatementType.Reference ``type``
            Open = []
            CodeItems = ``type`` |> List.map vmTypeIdentifier 
            NestedStatements = []
            PostCodeItems = []
        }

    let unitType =
        {
            Type = FsStatementType.Unit
            Open = []
            CodeItems = [vmType "unit"]
            NestedStatements = []
            PostCodeItems = []
        }

    let objType =
        {
            Type = FsStatementType.Primitive
            Open = []
            CodeItems = [vmType "obj"]
            NestedStatements = []
            PostCodeItems = []
        }

    let isAnonymousType s =
        match s.Type with
        | FsStatementType.Anonymous -> true
        | _ -> false

    let isReference s =
        match s.Type with
        | FsStatementType.Reference _ -> true
        | _ -> false


    let rec codeItems (statement: InnerFsStatement) =
        statement.CodeItems @ (statement.NestedStatements |> List.map (fun ns -> ns |> codeItems) |> List.concat) @ statement.PostCodeItems

    let rec opens (statement: InnerFsStatement) : string list =
        statement.Open 
        @ (
            statement.NestedStatements
            |> List.map opens
            |> List.concat
        )
        |> List.distinct


module internal FsStatementV2 =

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


    let inner = function
        | FsStatementV2.InnerFsStatement s -> s |> Some
        | _ -> None

    let topLevel = function
    | FsStatementV2.TopLevelFsStatement s -> s |> Some
    | _ -> None
    

    //let toArray statement =
    //    match statement.Kind with
    //    | FsStatementKind.Type t ->
    //        {statement with 
    //            Kind = FsStatementKind.Type (FsStatementType.Array t)
    //            NestedStatements = statement.NestedStatements @ [arrayType]}
    //    | _ -> statement


    //let rec addLineBreak statement =
    //    match statement.NestedStatements with
    //    | [] when (statement.CodeItems |> List.last).Tag <> Tag.EndOfLine  ->
    //        {statement with CodeItems = statement.CodeItems @ [vmEndLineNull]}
    //    | [] -> statement
    //    | _ ->
    //        {statement with NestedStatements = statement.NestedStatements[..^1] @ [(addLineBreak (statement.NestedStatements |> List.last))]}


    let rec addLineBreak (statement: FsStatementV2) =
        match statement with
        | FsStatementV2.InnerFsStatement inner ->
            {inner with PostCodeItems = inner.PostCodeItems @ [vmEndLineNull]} |> FsStatementV2.InnerFsStatement
        | FsStatementV2.TopLevelFsStatement inner ->
            {inner with PostCodeItems = inner.PostCodeItems @ [vmEndLineNull]} |> FsStatementV2.TopLevelFsStatement


    let isInterface s =
        match s.Kind with
        | FsStatementKind.DU _
        | FsStatementKind.AbstractClass _ -> true
        | _ -> false


    let notHidden s = not (s.Hidden)


    let isLet s =
        match s.Kind with
        | FsStatementKind.LetImport _
        | FsStatementKind.LetImportDefault _ -> true
        | _ -> false


    let isLetImport s =
        match s.Kind with
        | FsStatementKind.LetImport _ -> true
        | _ -> false

    let isLetImportDefault s =
        match s.Kind with
        | FsStatementKind.LetImportDefault _ -> true
        | _ -> false


    let isField s =
        match s with
        | FsStatementV2.TopLevelFsStatement s' ->
            match s'.Kind with
            | FsStatementKind.Field _ -> true
            | _ -> false
        | _ -> false


    let field s =
        match s with
        | FsStatementV2.TopLevelFsStatement s' ->
            match s'.Kind with
            | FsStatementKind.Field _ -> s' |> Some
            | _ -> None
        | _ -> None


module internal TopLevelFsStatement =
    
    let opens (statements: TopLevelFsStatement list) : string list =
        let rec opens (statement: TopLevelFsStatement) : string list =
            statement.Open
            @ (
                statement.NestedStatements
                |> List.map (fun s ->
                    match s with
                    | FsStatementV2.TopLevelFsStatement s' -> opens s'
                    | FsStatementV2.InnerFsStatement s' -> InnerFsStatement.opens s'
                )
                |> List.concat
            )

        statements
        |> List.map opens
        |> List.concat
        |> List.distinct


    let rec openCodeItems (statements: TopLevelFsStatement list) (except: #seq<_>) =
        //let rec opens statement =
        //    statement.Open @ (statement.NestedStatements |> List.map (fun n -> opens n) |> List.concat)

        statements
        |> opens
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


    let rec codeItems (statement: TopLevelFsStatement) : CodeItem list =
        statement.Summary
        @ statement.CodeItems
        @ (
            statement.NestedStatements
            |> List.map (fun s ->
                match s with
                | FsStatementV2.TopLevelFsStatement s' -> codeItems s'
                | FsStatementV2.InnerFsStatement s' -> InnerFsStatement.codeItems s'
            )
            |> List.concat
        )
        @ statement.PostCodeItems


    let rec increaseTab (statement: TopLevelFsStatement) =
        {
            statement with
                Summary = CodeItem.increaseTab statement.Summary
                CodeItems = CodeItem.increaseTab statement.CodeItems
                NestedStatements = 
                    statement.NestedStatements 
                    |> List.map (function
                        | TopLevelFsStatement s -> increaseTab s |> FsStatementV2.TopLevelFsStatement
                        | InnerFsStatement inner -> inner  |> FsStatementV2.InnerFsStatement
                    )
                PostCodeItems = CodeItem.increaseTab statement.PostCodeItems
        }

    //let add statementA statementB =
    //    let space = [vmText " "]

    //    match statementA, statementB with
    //    | a, b when a = zeroType && b = zeroType -> zeroType
    //    | _, b when b = zeroType -> statementA
    //    | a, _ when a = zeroType -> statementB
    //    //| {Identifier = FsStatmentKind.Type }, {Identifier = FsStatmentKind.Type} ->
    //    //    {
    //    //        Identifier = FsStatmentKind.Type
    //    //        Scope = Inherit
    //    //        Open = statementA.Open @ statementB.Open
    //    //        CodeItems = statementA.CodeItems @ space @ statementB.CodeItems
    //    //        NestedStatements = statementA.NestedStatements @ statementB.NestedStatements
    //    //    }
    //    | _ -> failwith "Not implemented"
        

type TopLevelFsStatement with
    //static member (+) (statementA: TopLevelFsStatement, statementB: TopLevelFsStatement) =
    //    FsStatementV2.add statementA statementB
    static member CollectCodeItems(fsStatement) = TopLevelFsStatement.codeItems fsStatement
        
type FsStatementV2 with
    static member CollectCodeItems(fsStatement) =
        match fsStatement with
        | FsStatementV2.TopLevelFsStatement s -> TopLevelFsStatement.codeItems s
        | FsStatementV2.InnerFsStatement s -> InnerFsStatement.codeItems s

module Scope =
    let isMainModule = function
        | Scope.Module (ModuleScope.Main) -> true
        | _ -> false