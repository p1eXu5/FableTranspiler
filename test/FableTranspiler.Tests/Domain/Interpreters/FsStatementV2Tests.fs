namespace FableTranspiler.Tests.Interpreters

open FableTranspiler.SimpleTypes

module FsStatementV2Tests =

    open NUnit.Framework
    open FsUnit
    open FableTranspiler.Interpreters
    open FableTranspiler.Interpreters.FsInterpreter

    //[<Test>]
    //let ``addition of zeroType and zeroType equals to zeroType`` () =
    //    FsStatementV2.zeroType + FsStatementV2.zeroType |> should equal FsStatementV2.zeroType

    //[<Test>]
    //let ``addition of someType and zeroType equals to someType`` () =
    //    let someType =
    //        {
    //            Kind = FsStatementKind.Type FsStatementType.Primitive
    //            Scope = Inherit
    //            Open = []
    //            CodeItems = [vmType "bool"]
    //            NestedStatements = []
    //            PostCodeItems = []
    //            Summary = []
    //            Hidden = false
    //        }
    //    someType + FsStatementV2.zeroType |> should equal someType


    //[<Test>]
    //let ``isFieldListType test``() =
    //    let someType =
    //        {
    //            Kind = FsStatementKind.Type (FsStatementType.FieldList [Identifier "foo"])
    //            Scope = Inherit
    //            Open = []
    //            CodeItems = []
    //            NestedStatements = []
    //            PostCodeItems = []
    //            Summary = []
    //            Hidden = false
    //        }

    //    someType |> FsStatementV2.isFieldListType |> should be True