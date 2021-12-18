module rec FableTranspiler.Parsers.Types

open Microsoft.FSharp.Quotations


type Identifier = Identifier of string with
    static member Create(v) = Identifier v

type StringLiteral = StringLiteral of string with
    static member Create(v) = StringLiteral v

type ModulePath = ModulePath of string with
    static member Create(v) = ModulePath v


type ImportEntity =
    | No
    | Named of Identifier
    | Aliased of name: Identifier * alias: Identifier
    | All
    /// <summary>
    /// For example:
    /// <c>* as alias</c>
    /// </summary>
    | AllAliased of alias: Identifier

type ImportModule =
    | NodeModule of ModulePath
    | Relative of ModulePath


type Statement =
    | Const of Expression
    | Import of ImportEntity list * modulePath: ImportModule

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