namespace FableTranspiler.Tests.Parsers

open NUnit.Framework
open FableTranspiler.Parsers.Types
open FableTranspiler.Parsers

[<Category("Parsers.MiscTests")>]
module MiscTests =

    open FableTranspiler.Parsers
    open FParsec
    open FableTranspiler.Tests.Parsers.Common


    [<Test>]
    let ``declare const test`` () =
        let input = "declare const scrollSpy: ScrollSpy;"
        let i = Identifier.Create "scrollSpy"
        let typeDef = Dsl.DTsTypes.plainType ["ScrollSpy"] |> TypeDefinition.Single
        let expected = Statement.DeclareConst (i, typeDef)

        let result = run Identifier.statement input
        result |> shouldSuccess expected


    [<Test>]
    let ``declare const followed by export out default test`` () =
        let input = """
            declare const scrollSpy: ScrollSpy;
            export default scrollSpy;
        """
        let i = Identifier.Create "scrollSpy"
        let typeDef = Dsl.DTsTypes.plainType ["ScrollSpy"] |> TypeDefinition.Single
        let declareConst = Statement.DeclareConst (i, typeDef)
        let export =
            "scrollSpy"
            |> Identifier.Create
            |> ExportStatement.OutDefault
            |> Statement.Export

        let expected = [declareConst; export]

        let result = Identifier.document input
        result  |> beOk expected

