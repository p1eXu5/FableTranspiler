namespace FableTranspiler.Tests.VmAdapters

open FableTranspiler.Tests
open NUnit.Framework
open FsUnit
open FableTranspiler
open FableTranspiler.Parsers
open FableTranspiler.Interpreters
open FableTranspiler.Tests.VmAdapters.TestCaseSources.InterfaceTestCaseSources
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Parsers.Types
open FableTranspiler.Tests.Common
open FableTranspiler.Tests.Factories

module FsDocumentInterpreterTests =


    let private config = 
        InterpretConfigFactory.build (MockLoggerFactory.GetMockedLoggerFactory()) (fun _ -> None) FsCodeStyle.Fable


    


    let private interpreter modulePath fileContent =
        let statements = 
            match Parser.run fileContent with
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
        do
            interpreter modulePath input
            |> Interpreter.run config
            |> ignore

        let actual = config.Store.Get modulePath (Identifier "Foo")
        actual |> should be (ofCase <@ Some FsStatement.Typed @>)

        let actualDisplay = actual |> Option.map (fun statement -> statement.StringContent()) |> Option.defaultValue ""
        actualDisplay |> should contain "type Foo ="
        actualDisplay |> should contain "abstract smooth : U2<bool, string> option"


    [<Test>]
    let ``uses existing statement when there is an reference in type alias`` () =
        let modulePath = "test.d.ts" |> modulePath
        let input = """
            export interface Foo {
                smooth?: boolean | string | undefined;
            }

            export type LinkProps = Foo & React.HTMLProps<HTMLButtonElement>;
        """

        do
            interpreter modulePath input
            |> Interpreter.run config
            |> ignore

        let actual = config.Store.Get modulePath (Identifier "LinkProps")
        actual |> shouldL be (ofCase <@ Some FsStatement.Typed @>) "LinkProps type must be present in store:"

        let actualDisplay = actual |> Option.map (fun statement -> statement.StringContent()) |> Option.defaultValue ""
        actualDisplay |> should contain "type LinkProps ="
        actualDisplay |> shouldL contain "abstract smooth : U2<bool, string> option" "field presentation"




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


