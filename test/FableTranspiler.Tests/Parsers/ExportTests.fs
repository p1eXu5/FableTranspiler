namespace FableTranspiler.Tests.Parsers

open NUnit.Framework
open FsUnit
open FableTranspiler.Tests.Common.FsUnit
open FsToolkit.ErrorHandling
open FableTranspiler.Domain

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
        result |> shouldSuccessEqual expected


    [<TestCaseSource(typeof<TestCases>, nameof TestCases.ExportCases)>]
    let ``import statements - module name test`` (content: string, expected: Statement) =
        let result = run Export.statement content
        result |> shouldSuccessEqual expected

    [<Test>]
    let ``export import test`` () =
            let statement =
                """
                export import ActionAccessibility = __MaterialUI.SvgIcon; // require('material-ui/svg-icons/action/accessibility');
                """
            let result = run Export.statement statement
            result |> should be (ofCase <@ ParserResult<Statement, unit>.Success @>)
