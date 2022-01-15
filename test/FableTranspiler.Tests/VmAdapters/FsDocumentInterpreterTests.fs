namespace FableTranspiler.Tests.VmAdapters

open NUnit.Framework
open FsUnit
open FableTranspiler.Parsers
open FableTranspiler.VmAdapters
open FableTranspiler.VmAdapters.DocumentSegmentViewModel
open FableTranspiler.VmAdapters.FsInterpreter

module FsDocumentInterpreterTests =


    let store : FsStatementStore = FsStatementInMemoryStore.store

    [<Test>]
    let ``interpret exported interface statement with union field type test`` () =
        let input = """
            export interface Foo {
                smooth?: boolean | string | undefined;
            }
        """

        let statement = 
            match Parser.document input with
            | Ok s -> s
            | Error err -> AssertionException(err) |> raise

        let expected =
            [
                vmType "type"
                vmText "Foo"
            ]

        let vm = FsInterpreter.interpret None "test" store statement
        vm.Head.FsStatement.Content() |> should be (equal expected)
        ()



