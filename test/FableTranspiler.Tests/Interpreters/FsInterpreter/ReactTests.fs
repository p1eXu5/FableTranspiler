namespace FableTranspiler.Tests.Domain.Interpreters.FsInterpreter

open FableTranspiler.Tests
open NUnit.Framework
open FsUnit
open FableTranspiler
open FableTranspiler.Parsers
open FableTranspiler.Interpreters
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Interpreters.FsInterpreter.React
open FableTranspiler.Interpreters.FsInterpreter.Facade
open FableTranspiler.Parsers.Types
open FableTranspiler.Tests.Common.FsUnit
open FableTranspiler.Tests.Common.SimpleTypesFactories
open FableTranspiler.Tests.Factories
open FsToolkit.ErrorHandling

module ReactTests =

    let private strategy : InterpretStrategy = 
        { InterpretInterface = React.interpretInterface }

    let [<Literal>] rootFullPath = 
        @"D:\Projects\Programming\FSharp\_wpf\FableTranspiler\node_modules\@types\react-scroll" 

    let [<Literal>] moduleFullPath = 
        @"D:\Projects\Programming\FSharp\_wpf\FableTranspiler\node_modules\@types\react-scroll\modules\components\Link.d.ts"

    let toFsStatement statement = 
        result {
            let! rootFullPath' = rootFullPath |> FullPath.Create
            let! moduleFullPath' = moduleFullPath |> FullPath.Create
            return toFsStatement strategy rootFullPath' moduleFullPath' statement
        }

    let private interpreter modulePath fileContent =
        let statements = 
            match Parser.run fileContent with
            | Ok s -> s
            | Error err -> AssertionException(err) |> raise

        Facade.interpret None modulePath statements


    [<Test>]
    let ``interpret interface produces statement with expected identifier`` () =
        result {
            let! statements =
                """
                    export interface Foo {
                        smooth?: boolean | string | undefined;
                    }
                """
                |> Parser.run

            let! fsStatement =
                toFsStatement statements.Head

            fsStatement 
            |> FsStatementV2.identifier 
            |> shouldL equal (statements.Head |> Statement.identifier) "Wrong identifier"
        } 
        |> Result.runTest


    [<TestCaseSource(typeof<ReactTestCases>, nameof ReactTestCases.FieldTests)>]
    let ``interface field interpretation tests`` (input: string, expected: string) =
        result {
            let! statements = input |> Parser.run
            let! fsStatement = toFsStatement statements.Head
            $"%O{fsStatement}" |> shouldL contain expected ""
        }
        |> Result.runTest


    [<Test>]
    let ``interpret interface produces statement with expected line breaks`` () =
        result {
            let! statements =
                """
                    export interface Foo {
                        to?: string;
                        smooth?: boolean | string | undefined;
                        onClick?(): void;
                    }
                """
                |> Parser.run

            let! fsStatement =
                toFsStatement statements.Head

            fsStatement 
            |> FsStatementV2.codeItems
            |> List.filter ((=) vmEndLineNull)
            |> shouldL haveLength 4 "Wrong count of vmEndLineNull"
        } 
        |> Result.runTest


    //[<Test>]
    //let ``uses existing statement when there is an reference in type alias`` () =
    //    let modulePath = "test.d.ts" |> ModulePath.createUnsafe
    //    let input = """
    //        export interface Foo {
    //            smooth?: boolean | string | undefined;
    //        }

    //        export type LinkProps = Foo & React.HTMLProps<HTMLButtonElement>;
    //    """

    //    do
    //        interpreter modulePath input
    //        |> Interpreter.run config
    //        |> ignore

    //    let actual = config.Store.Get modulePath (Identifier "LinkProps")
    //    actual |> shouldL be (ofCase <@ Some FsStatement.Typed @>) "LinkProps type must be present in store:"

    //    let actualDisplay = actual |> Option.map (fun statement -> statement.StringContent()) |> Option.defaultValue ""
    //    actualDisplay |> should contain "type LinkProps ="
    //    actualDisplay |> shouldL contain "abstract smooth : U2<bool, string> option" "field presentation"




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


