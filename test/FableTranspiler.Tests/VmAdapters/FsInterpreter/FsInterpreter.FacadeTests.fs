namespace FableTranspiler.Tests.VmAdapters

open NUnit.Framework
open FsUnit
open FableTranspiler.Parsers
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters.FsInterpreter
open FableTranspiler.Tests.VmAdapters.TestCaseSources.InterfaceTestCaseSources

module FsDocumentInterpreterTests =

    let store : FsStatementStore = FableTranspiler.Infrastruture.FsStatementInMemoryStore.store


    [<Test>]
    let ``interpret exported interface statement with union option field type to Fable with expected Name test`` () =
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
        vm[1].StyledFsStatements.Name() |> should equal "Foo" // [0] - is "module ..."


    [<TestCaseSource(typeof<TestCases>, nameof TestCases.ExportCases)>]
    let ``interpret exported interface statement with union option field type to Fable test`` (input: string, expectedField: string) =
        let statement = 
            match Parser.document input with
            | Ok s -> s
            | Error err -> AssertionException(err) |> raise


        let fableInterpreters =
            {
                InterpretPlainFableInterface = Fable.interpretPlainFableInterface
            }

        let vm = Facade.interpret None "test" store fableInterpreters statement

        vm[1].StyledFsStatements.StringContent() |> should haveSubstring expectedField


