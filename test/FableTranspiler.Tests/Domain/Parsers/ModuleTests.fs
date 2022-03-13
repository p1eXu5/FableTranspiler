namespace FableTranspiler.Tests.Parsers

open NUnit.Framework

[<Category("Parsers.CommentTests")>]
module ModuleTests =

    open FsUnit
    open FableTranspiler.Tests.Common.FsUnit
    open FableTranspiler.Parsers
    open FableTranspiler.Parsers.Types
    open FParsec
    open FableTranspiler.Tests.Common


    [<TestCase(@"'./foo'")>]
    [<TestCase(@"""./foo""")>]
    [<TestCase(@"'./foo'")>]
    [<TestCase(@"""./foo""")>]
    let ``relative module tests`` (path: string) =
        run Module.relative path
        |> shouldSuccess be (ofCase <@ DtsModule.Relative @>)


    [<TestCase(@"'~/foo'")>]
    [<TestCase(@"""~/foo""")>]
    [<TestCase(@"'node_modules/foo'")>]
    [<TestCase(@"""node_modules/foo""")>]
    let ``node module tests`` (path: string) =
        run Module.node path
        |> shouldSuccess be (ofCase <@ DtsModule.NodeModule @>)
