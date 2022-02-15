namespace FableTranspiler.Domain

open System
open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types

type ParsingResult =
    {
        ModulePath: ModulePath
        Statements: Result<StatementList, string>
    }

type FileParsingResultTree =
    | Leaf of ParsingResult
    | Branch of ParsingResult * FileParsingResultTree list
