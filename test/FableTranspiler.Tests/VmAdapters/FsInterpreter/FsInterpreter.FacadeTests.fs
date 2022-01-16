namespace FableTranspiler.Tests.VmAdapters

open NUnit.Framework
open FsUnit
open FableTranspiler.Parsers
open FableTranspiler.VmAdapters
open FableTranspiler.VmAdapters.FsInterpreter

module FsDocumentInterpreterTests =

    let store : FsStatementStore = FableTranspiler.Infrastruture.FsStatementInMemoryStore.store

    [<Test>]
    let ``interpret exported interface statement with union option field type to Fable test`` () =
        let input = """
            export interface Foo {
                smooth?: boolean | string | undefined;
            }
        """

        let statement = 
            match Parser.document input with
            | Ok s -> s
            | Error err -> AssertionException(err) |> raise


        let fableInterpreters =
            {
                InterpretPlainFableInterface = Fable.interpretPlainFableInterface
            }

        let vm = Facade.interpret None "test" store fableInterpreters statement
        vm[1].FsStatement.Name() |> should equal "Foo" // [0] - is "module ..."

        let expectedField = "abstract smooth : U2<bool, string> option"
        vm[1].FsStatement.StringContent() |> should haveSubstring expectedField


