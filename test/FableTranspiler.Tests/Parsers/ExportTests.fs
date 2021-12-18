namespace FableTranspiler.Tests.Parsers

open NUnit.Framework

[<Category("Parsers.ExportTests")>]
module ExportTests =

    open FableTranspiler.Parsers
    open FableTranspiler.Parsers.Types
    open FParsec
    open FableTranspiler.Tests.Parsers.Common


    [<TestCase("export = ReactScroll;")>]
    [<TestCase("export=ReactScroll;")>]
    let ``export statement test`` (input: string) =
        let result = run Export.exportStatement input
        let expected = Statement.Export (Identifier.Create "ReactScroll")
        result |> shouldSuccess expected