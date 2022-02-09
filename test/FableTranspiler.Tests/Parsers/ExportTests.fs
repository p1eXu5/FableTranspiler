namespace FableTranspiler.Tests.Parsers

open NUnit.Framework

[<Category("Parsers.ExportTests")>]
module ExportTests =

    open FableTranspiler.Parsers
    open FableTranspiler.Parsers.Dsl
    open FableTranspiler.Parsers.Types
    open FParsec
    open FableTranspiler.Tests.Common


    [<TestCase("export = ReactScroll;")>]
    [<TestCase("export=ReactScroll;")>]
    let ``export statement test`` (input: string) =
        let result = run Export.statement input
        let expected = Export.outAssignment "ReactScroll"
        result |> shouldSuccess expected


    [<TestCaseSource(typeof<TestCases>, nameof TestCases.ExportCases)>]
    let ``import statements - module name test`` (content: string, expected: Statement) =
        let result = run Export.statement content
        result |> shouldSuccess expected