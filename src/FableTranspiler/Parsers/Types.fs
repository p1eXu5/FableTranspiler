module rec FableTranspiler.Parsers.Types

open Microsoft.FSharp.Quotations


type Identifier = Identifier of string with
    static member Create(v) = Identifier v

type StringLiteral = StringLiteral of string with
    static member Create(v) = StringLiteral v

type ModulePath = ModulePath of string with
    static member Create(v) = ModulePath v


type TypeAlias =
    | Composition
    | Union

type StructureStatement =
    | Type
    | Class
    | Interface


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
    | DefaultAliased of alias: Identifier


type ExportStatement =
    | OutAssignment of Identifier
    | OutList of Identifier list
    | Transit of ExportEntity list * DtsModule



type Statement =
    | Comment of string
    | Import of ImportEntity list * modulePath: DtsModule
    | Export of ExportStatement
    | Const of Expression



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