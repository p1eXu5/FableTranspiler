module FableTranspiler.AppTypes

open FableTranspiler.Parsers.Types

type ParsingResult =
    {
        Path: string
        Statements: Result<StatementList, string>
    }

type FileParsingResultTree =
    | Leaf of ParsingResult
    | Branch of ParsingResult * FileParsingResultTree list