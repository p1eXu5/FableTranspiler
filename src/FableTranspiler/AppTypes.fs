module FableTranspiler.AppTypes

open FableTranspiler.Parsers.Types
open ConstrainedTypes

//type ModulePath = private ModulePath of string with
//    static member Create(v) = ConstrainedString.Create((nameof ModulePath), ModulePath, 5, 255, v)
//    static member Value(ModulePath v) = v


type ParsingResult =
    {
        Path: string
        Statements: Result<StatementList, string>
    }

type FileParsingResultTree =
    | Leaf of ParsingResult
    | Branch of ParsingResult * FileParsingResultTree list