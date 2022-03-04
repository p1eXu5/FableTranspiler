module rec FableTranspiler.Parsers.Types

open FableTranspiler.SimpleTypes
open System.Text


type StringLiteral = StringLiteral of string with
    static member Create(v) = StringLiteral v
    static member Value(StringLiteral v) = v


type DTsType =
    | Void
    | Undefined
    | Any
    | Func of parameters: FieldList * returnType: TypeDefinition
    | Plain of Identifier list
    | Generic of Identifier list * DTsType list
    | Typeof of Identifier list
    | Array of DTsType
    | InlineObject of FieldList
    with
        static member Identifier(dtsType) =
            match dtsType with
            | Plain is | Generic (is, _) | Typeof is -> is |> Some 
            | _ -> None

        override this.ToString() =
            match this with
            | Generic (idList, typeList) ->
                let sb =
                    StringBuilder()
                        .Append(
                            idList
                            |> List.map (Identifier.Value)
                            |> List.reduce (fun i1 i2 -> i1 + "." + i2)
                        )
                        .Append("<")

                typeList 
                |> List.map (sprintf "%O")
                |> List.iter (fun s -> sb.Append(s) |> ignore)

                sb.Append(">").ToString()

            | Plain idList ->
                idList
                |> List.map (Identifier.Value)
                |> List.reduce (fun i1 i2 -> i1 + "." + i2)

            | Typeof idList ->
                (+) 
                    "typeof "
                    (idList
                    |> List.map (Identifier.Value)
                    |> List.reduce (fun i1 i2 -> i1 + "." + i2))

            | _ -> $"%A{this}".ToLowerInvariant()


type TypeCombination =
    /// "type & type"
    | Composition of DTsType list
    /// "type | type"
    | Union of DTsType list


type TypeDefinition =
    | Combination of TypeCombination
    | Single of DTsType

type Field =
    | Required of Identifier
    | Optional of Identifier
    | FuncOpt of Identifier * FieldList
    | FuncReq of Identifier * FieldList

type FieldList = (Field * TypeDefinition) list


type ClassDefinition =
    | ExtendsEmpty of Identifier * DTsType

type InterfaceDefinition =
    | Extends of Identifier * DTsType * FieldList
    | Plain of Identifier * FieldList


type TypeAlias =
    | Plain of Identifier * TypeCombination
    | Generic of name: Identifier * typeParam: Identifier list * TypeCombination


type FunctionDefinition =
    | Plain of name: Identifier * parameters: FieldList * returnType: TypeDefinition
    | Generic of name: Identifier * typeParams: Identifier list * parameters: FieldList * returnType: TypeDefinition
    | GenericNameless of typeParams: Identifier list * parameters: FieldList * returnType: TypeDefinition


type ConstDefinition =
    | DeclareConst of Identifier * TypeDefinition
    | Const of Identifier * TypeDefinition


type StructureStatement =
    | TypeAlias of TypeAlias
    | ClassDefinition of ClassDefinition
    | InterfaceDefinition of InterfaceDefinition
    | FunctionDefinition of FunctionDefinition
    | ConstDefinition of ConstDefinition
    // | TypeDefinition of Identifier * TypeDefinition




type ImportEntity =
    | No
    | Named of Identifier
    /// <summary>
    /// For example:
    /// <c>Foo as Bar</c>
    /// </summary>
    | Aliased of name: Identifier * alias: Identifier
    /// <summary>
    /// For example:
    /// <c>*</c>
    /// </summary>
    | All
    /// <summary>
    /// For example:
    /// <c>* as alias</c>
    /// </summary>
    | AllAliased of alias: Identifier
    with
        override this.ToString() =
            match this with
            | No -> ""
            | Named (Identifier id) -> id
            | Aliased ((Identifier name), (Identifier alias)) -> $"{name} as {alias}"
            | All -> "*"
            | AllAliased (Identifier alias) -> $"* as {alias}"

type DtsModule =
    | NodeModule of ModulePath
    | Relative of ModulePath
    with
        override this.ToString() =
            match this with
            | NodeModule (ModulePath path) 
            | Relative (ModulePath path) -> $"'{path}'"



type ExportEntity =
    | Named of Identifier
    /// "...default as Alias..."
    | DefaultAliased of alias: Identifier


type ExportStatement =
    /// "export = ReactScroll;"
    | OutAssignment of Identifier

    /// "export { animateScroll, scroller };"
    | OutList of Identifier list

    /// "export default scrollSpy;"
    | OutDefault of Identifier

    /// "export { default as ScrollLink, ScrollLinkProps } from './mixins/scroll-link';"
    | Transit of ExportEntity list * DtsModule
    | Structure of StructureStatement
    | StructureDefault of StructureStatement
    | Namespace of Identifier * StatementList



type Statement =
    | Comment of string
    | Import of ImportEntity list * modulePath: DtsModule
    | Export of ExportStatement
    | Const of Expression
    | Structure of StructureStatement
    | NamespaceDeclaration of Identifier * StatementList
    with
        override this.ToString() =
            match this with
            | Comment comment -> comment
            | Import (entities, modulePath) ->
                let sb = StringBuilder().Append("import ")

                if entities.Length > 1 then
                    sb.Append("{") |> ignore
                
                sb.Append(
                    entities
                    |> List.map (fun e -> e.ToString())
                    |> List.reduce (fun e1 e2 -> $"%O{e1}, %O{e2}")
                )
                |> ignore

                if entities.Length > 1 then
                    sb.Append("}") |> ignore

                sb.Append(" from ").Append(modulePath.ToString()).Append(";").ToString()
            | _ -> $"%A{this}"


and
    [<RequireQualifiedAccess>]
    Expression =
        | Empty
        | IntLiteral of int
        | FloatLiteral of float
        | StringLiteral of StringLiteral
        | ObjectLiteral of Expression list
        
        | Identifier of Identifier
        | Function of idetifier: string * Expression
        | Binary of (ExpressionKind * Expression * Expression)
    and
        ExpressionKind =
            | Add
            | Or
            | Assignment
            | Dereferentiation
            | Typification
            | Invokation
            | AsCast
            | Undefined

type StatementList = Statement list




module StructureStatement =

    let name = function
        | TypeAlias (TypeAlias.Plain (i, _)) 
        | TypeAlias (TypeAlias.Generic (i, _, _))
        | ClassDefinition (ClassDefinition.ExtendsEmpty (i, _))
        | InterfaceDefinition (InterfaceDefinition.Extends (i, _, _))
        | InterfaceDefinition (InterfaceDefinition.Plain (i, _))
        | FunctionDefinition (FunctionDefinition.Plain (i, _, _))
        | FunctionDefinition (FunctionDefinition.Generic (i, _, _, _))
        | ConstDefinition (ConstDefinition.DeclareConst (i, _))
        | ConstDefinition (ConstDefinition.Const (i, _)) -> i |> Some
        | _ -> None


module Statement =

    let identifier = function
        | NamespaceDeclaration (identifier, _) -> identifier |> Some
        | Structure ss -> ss |> StructureStatement.name
        | Export (ExportStatement.Structure ss)
        | Export (ExportStatement.StructureDefault ss) -> ss |> StructureStatement.name
        | Export (ExportStatement.Namespace (i, _)) -> i |> Some 
        | _ -> None 

