namespace FableTranspiler.Tests.Interpreters

module FsStatementV2Tests =

    open NUnit.Framework
    open FsUnit
    open FableTranspiler.Interpreters
    open FableTranspiler.Interpreters.FsInterpreter

    [<Test>]
    let ``addition of zeroType and zeroType equals to zeroType`` () =
        FsStatementV2.zeroType + FsStatementV2.zeroType |> should equal FsStatementV2.zeroType

    [<Test>]
    let ``addition of someType and zeroType equals to someType`` () =
        let someType =
            {
                Identifier = FsStatmentKind.Type FsStatementType.Primitive
                Scope = Inherit
                Open = []
                CodeItems = [vmType "bool"]
                NestedStatements = []
            }
        someType + FsStatementV2.zeroType |> should equal someType