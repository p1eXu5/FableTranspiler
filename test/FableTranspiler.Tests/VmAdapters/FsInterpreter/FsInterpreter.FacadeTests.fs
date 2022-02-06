namespace FableTranspiler.Tests.VmAdapters

open NUnit.Framework
open FsUnit
open FableTranspiler.Parsers
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters.FsInterpreter
open FableTranspiler.Tests.VmAdapters.TestCaseSources.InterfaceTestCaseSources
open FableTranspiler.SimpleTypes
open FableTranspiler.VmAdapters.FsInterpreter.Types
open FableTranspiler.Parsers.Types

module FsDocumentInterpreterTests =

    let private store : FsStatementStore = FableTranspiler.Infrastruture.FsStatementInMemoryStore.store

    let private fableInterpreters =
        {
            InterpretPlainFableInterface = Fable.interpretPlainFableInterface
        }

    let private config = 
        {
            Interpreters = fableInterpreters
            Store = store
        }


    let modulePath fileName =
        match fileName |> ModulePath.Create with
        | Ok modulePath -> modulePath
        | Error err -> raise (AssertionException err)


    let private interpreter modulePath fileContent =
        let statements = 
            match Parser.document fileContent with
            | Ok s -> s
            | Error err -> AssertionException(err) |> raise

        Facade.interpret None modulePath statements


    [<Test>]
    let ``saves exported interface statement in store`` () =
        let modulePath = "test.d.ts" |> modulePath
        let input = """
            export interface Foo {
                smooth?: boolean | string | undefined;
            }
        """
        let expectedInterpretations =
            [
                "type Foo ="
                "abstract smooth : U2<bool, string> option"
            ]

        do
            interpreter modulePath input
            |> Interpreter.run config
            |> ignore

        let actual = store.Get modulePath (Identifier "Foo")
        actual |> should be (ofCase <@ Some @>)

        let actualDisplay = actual |> Option.map (fun statement -> statement.StringContent()) |> Option.defaultValue ""
        actualDisplay |> should contain expectedInterpretations[0]
        actualDisplay |> should contain expectedInterpretations[1]


    //[<Test>]
    //let ``exported interface interpretation test`` () =
    //    let fileName = "test.d.ts"
    //    let input = """
    //        export interface Foo {
    //            smooth?: boolean | string | undefined;
    //        }
    //    """

    //    let dtoList = interpret fileName input
    //    dtoList.Length |> should equal 2

    //    dtoList[0].StyledFsStatements.Length |> should equal 1
    //    dtoList[0].StyledFsStatements[0].FsCodeStyle |> should be (ofCase<@ FsCodeStyle.Universal @>)
    //    dtoList[0].StyledFsStatements[0].FsStatement |> should be (ofCase<@ FsStatement.Named @>)
    //    dtoList[0].StyledFsStatements[0].FsStatement |> FsStatement.name |> should be (ofCase<@ Some "Test" @>)

    //    dtoList[1].StyledFsStatements.Length |> should equal 1
    //    dtoList[1].StyledFsStatements[0].FsCodeStyle |> should be (ofCase<@ FsCodeStyle.Fable @>)
    //    dtoList[1].StyledFsStatements[0].FsStatement |> should be (ofCase<@ FsStatement.Interface @>)
    //    dtoList[1].StyledFsStatements[0].FsStatement |> FsStatement.name |> should be (ofCase<@ Some "Foo" @>)


    //[<Test>]
    //let ``exported type alias with react type interpretation test`` () =
    //    let fileName = "test.d.ts"
    //    let input = """
    //        import * as React from 'react';

    //        export interface Foo {
    //            smooth?: boolean | string | undefined;
    //        }

    //        export type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;
    //    """

    //    let dtoList = interpret fileName input
    //    dtoList.Length |> should equal 3 // module, type, type


    //[<TestCaseSource(typeof<TestCases>, nameof TestCases.ExportCases)>]
    //let ``interpret exported interface statement with union option field type to Fable test`` (input: string, expectedField: string) =
    //    let statement = 
    //        match Parser.document input with
    //        | Ok s -> s
    //        | Error err -> AssertionException(err) |> raise


    //    let fableInterpreters =
    //        {
    //            InterpretPlainFableInterface = Fable.interpretPlainFableInterface
    //        }

    //    let vm = Facade.interpret None "test" store fableInterpreters statement

    //    vm[1].StyledFsStatements[0].StringContent() |> should haveSubstring expectedField


