﻿module rec FableTranspiler.Parsers.Types

open Microsoft.FSharp.Quotations


type Identifier = Identifier of string with
    static member Create(v) = Identifier v
    static member Value(Identifier v) = v



type StringLiteral = StringLiteral of string with
    static member Create(v) = StringLiteral v
    static member Value(StringLiteral v) = v

type ModulePath = ModulePath of string with
    static member Create(v) = ModulePath v
    static member Value(ModulePath v) = v


type DTsType =
    | Void
    | Undefined
    | Any
    | Func of FieldList * TypeDefinition
    | Plain of Identifier list
    | Generic of Identifier list * DTsType list
    | Typeof of Identifier
    | Array of DTsType


type TypeCombination =
    | Composition of DTsType list
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


type StructureStatement =
    | TypeAlias of Identifier * TypeCombination
    | ClassDefinition of ClassDefinition
    | InterfaceDefinition of InterfaceDefinition
    | FunctionDefinition of  Identifier * FieldList * TypeDefinition
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



type Statement =
    | Comment of string
    | Import of ImportEntity list * modulePath: DtsModule
    | Export of ExportStatement
    | Const of Expression
    | DeclareConst of Identifier * TypeDefinition
    | Structure of StructureStatement



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

type Statements = Statement list