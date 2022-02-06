module rec FableTranspiler.Parsers.Types

open FableTranspiler.SimpleTypes

type Identifier = Identifier of string with
    static member Create(v) = Identifier v
    static member Value(Identifier v) = v


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
    | Typeof of Identifier
    | Array of DTsType
    | InlineObject of FieldList


type TypeCombination =
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

type DtsModule =
    | NodeModule of ModulePath
    | Relative of ModulePath



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