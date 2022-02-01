module FableTranspiler.AppTypes

open FableTranspiler.Parsers.Types

type ParsingResult =
    {
        Path: string
        Statements: Result<StatementList, string>
    }

type ModuleTreeParsingResult =
    | Leaf of ParsingResult
    | Branch of ParsingResult * ModuleTreeParsingResult list