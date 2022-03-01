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
open FableTranspiler.Ports.PortsBuilder
open System.IO

module ReactTests =

    let private strategy : InterpretStrategy = 
        { 
            InterpretInterface = React.interpretInterface 
            InterpretTypeAlias = React.interpretTypeAlias
        }

    let private config : InterpretConfigV2 =
        {
            InterpretStrategy = strategy
            StatementStore = FableTranspiler.Adapters.Persistence.StatementStore.create (Statement.identifier)
            FsStatementStore = FableTranspiler.Adapters.Persistence.StatementStore.create (FsStatementV2.identifier)
        }

    let [<Literal>] rootFullPath = 
        @"Z:\Projects\Programming\FSharp\_wpf\FableTranspiler\node_modules\@types\react-scroll" 


    let [<Literal>] testRelativePath = "./modules/components/Link"


    let fullPath relativePath =
        Path.GetFullPath(Path.Combine(rootFullPath, relativePath + ".d.ts")) |> FullPath.Create

    /// <summary>
    /// 
    /// </summary>
    /// <param name="relativePath"> './modules/components/Link' for example </param>
    /// <param name="statementList"></param>
    let private interpretV2' relativePath statementList = 
        result {
            let! rootFullPath' = rootFullPath |> FullPath.Create
            let! moduleFullPath' = Path.Combine(rootFullPath, relativePath) |> FullPath.Create
            return Ports.run config (interpretV2 rootFullPath' moduleFullPath' statementList)
        }

    let private toFsStatement' relativePath statement innerConfig = 
        result {
            let! rootFullPath' = rootFullPath |> FullPath.Create
            let! moduleFullPath' = Path.Combine(rootFullPath, relativePath) |> FullPath.Create
            return Ports.run config (toFsStatement rootFullPath' moduleFullPath' statement innerConfig)
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

            let! fsStatements =
                interpretV2' testRelativePath statements

            fsStatements[0]
            |> FsStatementV2.identifier 
            |> shouldL equal (statements.Head |> Statement.identifier) "Wrong identifier"
        } 
        |> Result.runTest


    [<TestCaseSource(typeof<ReactTestCases>, nameof ReactTestCases.FieldTests)>]
    let ``interface field interpretation tests`` (input: string, expected: string) =
        result {
            let! statements = input |> Parser.run
            let! fsStatements = interpretV2' testRelativePath statements
            $"%O{fsStatements[0]}" |> shouldL contain expected ""
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

            let! fsStatements =
                interpretV2' testRelativePath statements

            fsStatements[0]
            |> FsStatementV2.codeItems
            |> List.filter ((=) vmEndLineNull)
            |> shouldL haveLength 4 "Wrong count of vmEndLineNull"
        } 
        |> Result.runTest


    [<Test>]
    let ``toFsStatement: when interpret exported interface then store it`` () =
        result {
            let statementsResult =
                """
                    export interface Foo {
                        to?: string;
                        smooth?: boolean | string | undefined;
                        onClick?(): void;
                    }

                """
                |> Parser.run

            let! moduleP = fullPath testRelativePath
            config.StatementStore.TryAdd moduleP statementsResult |> ignore
            
            let! statements = statementsResult
                
            let! (_, conf) =
                toFsStatement' testRelativePath statements[0] None

            conf
            |> Option.bind (fun c ->
                c.TryGetLocal (Identifier.Create "Foo")
            )
            |> should be (ofCase <@ Some @>)
            
        }
        |> Result.runTest


    [<Test>]
    let ``toFsStatement: when interpret many exported interfaces then store their`` () =
        result {
            let statementsResult =
                """
                    export interface Foo {
                        to?: string;
                        smooth?: boolean | string | undefined;
                        onClick?(): void;
                    }

                    export interface Bar {
                        onSetActive?(to: string): void;
                        duration?: number | string | ((distance: number) => number) | undefined;
                    }

                """
                |> Parser.run

            let! moduleP = fullPath testRelativePath
            config.StatementStore.TryAdd moduleP statementsResult |> ignore
            
            let! statements = statementsResult
                
            let! (_, conf) =
                toFsStatement' testRelativePath statements[0] None
                |> Result.bind (fun (_, c') ->
                    toFsStatement' testRelativePath statements[1] c'
                )

            conf
            |> Option.bind (fun c ->
                c.TryGetLocal (Identifier.Create "Foo")
            )
            |> shouldL be (ofCase <@ Some @>) "Has no Foo"

            conf
            |> Option.bind (fun c ->
                c.TryGetLocal (Identifier.Create "Bar")
            )
            |> shouldL be (ofCase <@ Some @>) "Has no Bar"
            
        }
        |> Result.runTest


    [<Test>]
    let ``interpret type composition from two local interfaces produces fulfilled DU`` () =
        result {
            let statementsResult =
                """
                    export interface Foo {
                        to?: string;
                        smooth?: boolean | string | undefined;
                        onClick?(): void;
                    }

                    export interface Bar {
                        onSetActive?(to: string): void;
                        duration?: number | string | ((distance: number) => number) | undefined;
                    }

                    export type BazProps = Foo & Bar;
                """
                |> Parser.run

            let! moduleP = fullPath testRelativePath
            config.StatementStore.TryAdd moduleP statementsResult |> ignore

            let! statements = statementsResult

            let! fsStatements =
                statements
                |> interpretV2' testRelativePath

            fsStatements 
            |> shouldL haveLength 3 "Wrong count of fs statements"

            let bazProps = fsStatements |> List.last
            bazProps.NestedStatements[0].Identifier |> should equal (FsStatmentKind.Type FsStatementType.Composition)
            bazProps.NestedStatements[0].NestedStatements |> shouldL haveLength 2 "Wrong NestedStatements count"
            Assert.That(bazProps.NestedStatements[0].NestedStatements, Has.All.Property("Identifier").Matches( ofCase <@ FsStatmentKind.Type @> ))

            let present = bazProps.ToString()
            present |> should contain "| To of string"
            present |> should contain "| OnSetActive of (string -> unit)"
        } 
        |> Result.runTest


    [<Test>]
    let ``interpret type composition from two interfaces from importing modules produces fulfilled DU`` () =
        result {
            let statementsResultA =
                """
                    export interface Foo {
                        to?: string;
                        smooth?: boolean | string | undefined;
                        onClick?(): void;
                    }
                """
                |> Parser.run

            let! moduleA = fullPath "./Foo"
            config.StatementStore.TryAdd moduleA statementsResultA |> ignore


            let statementsResultB =
                """
                    import {Foo} from './Foo';

                    export interface Bar {
                        onSetActive?(to: string): void;
                        duration?: number | string | ((distance: number) => number) | undefined;
                    }

                    export type BazProps = Foo & Bar;
                """
                |> Parser.run

            let! moduleB = fullPath "./BazProps"
            config.StatementStore.TryAdd moduleB statementsResultB |> ignore


            let! statements = statementsResultB

            let! fsStatements =
                statements
                |> interpretV2' "./BazProps"

            fsStatements 
            |> shouldL haveLength 2 "Wrong count of fs statements"

            let bazProps = fsStatements |> List.last
            bazProps.NestedStatements[0].Identifier |> should equal (FsStatmentKind.Type FsStatementType.Composition)
            bazProps.NestedStatements[0].NestedStatements |> shouldL haveLength 2 "Wrong NestedStatements count"
            Assert.That(bazProps.NestedStatements[0].NestedStatements, Has.All.Property("Identifier").Matches( ofCase <@ FsStatmentKind.Type @> ))

            let present = bazProps.ToString()
            present |> should contain "| To of string"
            present |> should contain "| OnSetActive of (string -> unit)"
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


