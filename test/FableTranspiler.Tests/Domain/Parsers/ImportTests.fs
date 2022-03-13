namespace FableTranspiler.Tests.Parsers

open NUnit.Framework
open FableTranspiler.SimpleTypes
open FableTranspiler.Tests.Common.FsUnit

[<Category("Parsers.ImportTests")>]
module ImportTests =

    open FableTranspiler.Parsers
    open FableTranspiler.Parsers.Types
    open FParsec
    open FableTranspiler.Tests.Common


    [<TestCase("foo as Bar ")>]
    [<TestCase("foo as Bar")>]
    [<TestCase("foo as Bar;")>]
    let ``aliased test`` (input: string) =
        let result = run Import.aliased input
        let expected = ImportEntity.Aliased ((Identifier.create "foo"), (Identifier.create "Bar"))
        result |> shouldSuccessEqual expected


    [<TestCase("* ")>]
    let ``all test`` (input: string) =
        let result = run Import.all input
        let expected = ImportEntity.All
        result |> shouldSuccessEqual expected


    [<TestCase("* as React ")>]
    [<TestCase("* as React")>]
    [<TestCase("* as React;")>]
    let ``allAliased test`` (input: string) =
        let result = run Import.allAliased input
        let expected = ImportEntity.AllAliased (Identifier.create "React")
        result |> shouldSuccessEqual expected



    [<TestCaseSource(typeof<TestCases>, nameof TestCases.ImportCases)>]
    let ``import statements - module name test`` (content: string, expected: Statement) =
        let result = run Import.statement content
        result |> shouldSuccessEqual expected



    [<TestCase("'react'")>]
    [<TestCase("'~/react'")>]
    let ``nodeModule test`` (input) =
        let result = run Module.nodeModuleTilda input
        let expected = DtsModule.NodeModule (ModulePath (input.Replace("'", "")))
        result |> shouldSuccessEqual expected