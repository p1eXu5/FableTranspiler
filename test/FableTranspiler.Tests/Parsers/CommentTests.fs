namespace FableTranspiler.Tests.Parsers

open NUnit.Framework

[<Category("Parsers.CommentTests")>]
module CommentTests =

    open FableTranspiler.Parsers
    open FableTranspiler.Parsers.Types
    open FParsec
    open FableTranspiler.Tests.Common


    [<Test>]
    let ``comments test`` () =
        let input = """// sdfsfsfd// sdfsdf"""
        let result = run Comment.statement input
        let expected = Statement.Comment "// sdfsfsfd// sdfsdf"
        result |> shouldSuccess expected
