module FableTranspiler.AppTypes

open FableTranspiler.Parsers.Types
open SimpleTypes


type ParsingResult =
    {
        Path: ModulePath
        Statements: Result<StatementList, string>
    }

type FileParsingResultTree =
    | Leaf of ParsingResult
    | Branch of ParsingResult * FileParsingResultTree list