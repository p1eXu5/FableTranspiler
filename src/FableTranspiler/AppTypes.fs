module FableTranspiler.AppTypes

open FableTranspiler.Parsers.Types

type ParsingResult =
    {
        Path: string
        Statements: Result<StatementList, string>
    }

type ModuleTree =
    | Leaf of ParsingResult
    | Branch of ParsingResult * ModuleTree list