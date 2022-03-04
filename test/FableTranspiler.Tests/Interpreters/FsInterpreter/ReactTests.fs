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

    let private strategy : InterpretStrategy = Fable.strategy

    let private config : InterpretConfigV2 =
        {
            InterpretStrategy = strategy
            StatementStore = FableTranspiler.Adapters.Persistence.StatementStore.create (Statement.identifier)
            FsStatementStore = FableTranspiler.Adapters.Persistence.StatementStore.create (FsStatementV2.identifier)
        }

    let [<Literal>] rootFullPath = 
        @"Z:\Projects\Programming\FSharp\_wpf\FableTranspiler\node_modules\@types\react-scroll" 

    /// "./modules/components/Link"
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
            let! moduleFullPath' = fullPath relativePath
            return Ports.run config (interpretV2 rootFullPath' moduleFullPath' statementList)
        }


    let private toFsStatement' relativePath statement innerConfig = 
        result {
            let! rootFullPath' = rootFullPath |> FullPath.Create
            let! moduleFullPath' = Path.Combine(rootFullPath, relativePath) |> FullPath.Create
            return Ports.run config (toFsStatement rootFullPath' moduleFullPath' statement innerConfig)
        }


    let processDtsModule modulePath content =
        result {
            let statementsResult =
                content
                |> Parser.run

            let! fullModulePath = fullPath modulePath
            config.StatementStore.TryAdd fullModulePath statementsResult |> ignore
            return! statementsResult
        }


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
                    export interface FooProps {
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
            |> shouldL haveLength 5 "Wrong count of vmEndLineNull"
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
                    export interface FooProps {
                        to?: string;
                        smooth?: boolean | string | undefined;
                        onClick?(): void;
                    }

                    export interface BarProps {
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
                c.TryGetLocal (Identifier.Create "FooProps")
            )
            |> shouldL be (ofCase <@ Some @>) "Has no FooProps"

            conf
            |> Option.bind (fun c ->
                c.TryGetLocal (Identifier.Create "BarProps")
            )
            |> shouldL be (ofCase <@ Some @>) "Has no BarProps"
            
        }
        |> Result.runTest


    [<Test>]
    let ``interpret type composition from two local props interfaces produces fulfilled DU`` () =
        result {
            let statementsResult =
                """
                    export interface FooProps {
                        to?: string;
                        smooth?: boolean | string | undefined;
                        onClick?(): void;
                    }

                    export interface BarProps {
                        onSetActive?(to: string): void;
                        duration?: number | string | ((distance: number) => number) | undefined;
                    }

                    export type BazProps = FooProps & BarProps;
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
    let ``interpret Button_d_ts like module`` () =
        result {
            let! _ = 
                processDtsModule "./Link"
                    """
                        import * as React from 'react';

                        export interface ReactScrollLinkProps {
                            to?: string;
                        }

                        export type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;
                        export default class Link extends React.Component<LinkProps> {}
                    """

            let! buttonStatements = 
                processDtsModule "./Button"
                    """
                        import * as React from 'react';
                        import { ReactScrollLinkProps } from './Link';
                        
                        export type ButtonProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;
                        
                        export default class Button extends React.Component<ButtonProps> {}
                    """


            let! fsStatements =
                buttonStatements
                |> interpretV2' "./Button"

            fsStatements 
            |> shouldL haveLength 3 $"Wrong count of fs statements, %A{fsStatements}"

            let buttonProps = fsStatements |> List.item 1
            buttonProps.NestedStatements[0].Identifier |> should equal (FsStatmentKind.Type FsStatementType.Composition)
            buttonProps.NestedStatements[0].NestedStatements |> shouldL haveLength 1 "Wrong NestedStatements count"

            let present = buttonProps.ToString()
            present |> should contain "| To of string"
        } 
        |> Result.runTest


    [<Test>]
    let ``when interpret type combination with outer lib module props import then adds comments and summary`` () =
        result {
            let statementsResultB =
                """
                    import * as Foo from 'foo';

                    export interface BarProps {
                        onSetActive?(to: string): void;
                        duration?: number | string | ((distance: number) => number) | undefined;
                    }

                    export type BazProps = BarProps & Foo.FooProps<FooElement>;
                """
                |> Parser.run

            let! moduleFullPath = fullPath testRelativePath
            config.StatementStore.TryAdd moduleFullPath statementsResultB |> ignore


            let! statements = statementsResultB

            let! fsStatements =
                statements
                |> interpretV2' testRelativePath

            fsStatements 
            |> shouldL haveLength 3 "Wrong count of fs statements"

            fsStatements[0].Identifier |> should be (ofCase <@ FsStatmentKind.Comment @>)
            fsStatements[0].ToString() |> should contain "// outer lib is not processed yet - import * as Foo from 'foo';"

            let bazProps = fsStatements |> List.last
            bazProps.NestedStatements[0].Identifier |> should equal (FsStatmentKind.Type FsStatementType.Composition)
            bazProps.NestedStatements[0].NestedStatements |> shouldL haveLength 1 "Wrong NestedStatements count" // Foo.FooProps<FooElement> is unknown
            Assert.That(
                bazProps.NestedStatements[0].NestedStatements, 
                Has.All.Property("Identifier").Matches( ofCase <@ FsStatmentKind.Type @> ), 
                fun () -> "type alias assertion fails:" )

            let present = bazProps.ToString()
            present |> should contain "/// see also Foo.FooProps<FooElement>"
            present |> should contain "| OnSetActive of (string -> unit)"
            present |> should contain "| Duration of U3<float, string, (float -> float)>"
        } 
        |> Result.runTest


    [<Test>]
    let ``when export class with React.Component<FooProps> then adds open items`` () =
        ()

    [<Test>]
    let ``when export class with React Component then adds module with fs let statement`` () =
        result {
            let statementsResultB =
                """
                    import * as React from 'react';

                    export interface FooProps {
                        onSetActive?(to: string): void;
                        duration?: number | string | ((distance: number) => number) | undefined;
                    }

                    export type BarProps = FooProps & Foo.FooProps<FooElement>;

                    export default class Bar extends React.Component<BarProps> {}
                """
                |> Parser.run

            let! moduleFullPath = fullPath testRelativePath
            config.StatementStore.TryAdd moduleFullPath statementsResultB |> ignore

            let! statements = statementsResultB

            let! fsStatements =
                statements
                |> interpretV2' testRelativePath

            fsStatements 
            |> shouldL haveLength 4 "Wrong count of fs statements"

            let bar = fsStatements |> List.last
            bar.Open |> should contain "Fable.React"
            bar.Open |> should contain "Fable.Core.JsInterop"
            bar.Scope |> should equal (Scope.Module "Bar")

            let present = bar.ToString()
            // present |> should contain "module Bar ="
            present |> should contain "let inline Bar props children ="
            present |> should contain "domEl (importDefault \"react-scroll\\modules\\components\\Link\") props children"
        } 
        |> Result.runTest


    [<Test>]
    [<Ignore("namespace interpretation is needed to implement")>]
    let ``exported namespace interpretation test like scroll-events`` () =
        result {
            let! statements =
                processDtsModule "./scroll-events"
                    """
                        declare namespace Events {
                            interface ScrollEvent {
                                register(eventName: string, callback: (to: string, element: any) => void): void;
                                remove(eventName: string): void;
                            }
                        
                            const registered: {};
                            const scrollEvent: ScrollEvent;
                        }
                        
                        export default Events;
                    """

            let! fsStatements =
                statements
                |> interpretV2' "./scroll-events"

            fsStatements 
            |> shouldL haveLength 3 $"Wrong count of fs statements, %A{fsStatements}"

            fsStatements[0].Identifier |> should be (ofCase <@ FsStatmentKind.Interface @>)
            fsStatements[1].Identifier |> should be (ofCase <@ FsStatmentKind.Interface @>)
            fsStatements[2].Identifier |> should be (ofCase <@ FsStatmentKind.Object @>)
            fsStatements[2].Open |> should contain "Fable.Core"

            let sPresent0 = sprintf "%O" fsStatements[0]
            sPresent0 |> should contain "type ScrollEvent ="
            sPresent0 |> should contain "abstract register: eventName: string -> callback: (string -> obj -> unit) -> unit"
            sPresent0 |> should contain "abstract remove: eventName: string -> unit"

            let sPresent1 = sprintf "%O" fsStatements[1]
            sPresent1 |> should contain "type Events ="
            sPresent1 |> should contain "abstract registered : obj"
            sPresent1 |> should contain "abstract scrollEvent : ScrollEvent"

            let sPresent2 = sprintf "%O" fsStatements[1]
            sPresent2 |> should contain "[<ImportDefault(\"react-scroll/scroll-events\")>]"
            sPresent2 |> should contain "let events : Events = jsNative"
        }
        |> Result.runTest

