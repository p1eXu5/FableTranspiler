namespace FableTranspiler.Tests.Domain.Interpreters.FsInterpreter

open NUnit.Framework
open FsUnit
open FableTranspiler
open FableTranspiler.Parsers
open FableTranspiler.Interpreters
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Interpreters.FsInterpreter.Facade
open FableTranspiler.Parsers.Types
open FableTranspiler.Tests.Common.FsUnit
open FableTranspiler.Tests.Common.SimpleTypesFactories
open FsToolkit.ErrorHandling
open FableTranspiler.Ports.PortsBuilder
open System.IO
open NUnit.Framework

module ReactTests =

    let private strategy : InterpretStrategy = Fable.strategy

    let mutable private config : Result<InterpretConfigV2, string> = Result.Error ""

    [<SetUp>]
    let createConfig () =
        config <-
            {
                InterpretStrategy = strategy
                StatementStore = FableTranspiler.Adapters.Persistence.StatementStore.create (Statement.identifier)
                FsStatementStore = FableTranspiler.Adapters.Persistence.StatementStore.create (FsStatementV2.identifier)
            } |> Ok

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
            let! config' = config
            return Ports.run config' (interpretV2 rootFullPath' moduleFullPath' statementList None)
        }


    let private toFsStatement' relativePath statement innerConfig = 
        result {
            let! rootFullPath' = rootFullPath |> FullPath.Create
            let! moduleFullPath' = Path.Combine(rootFullPath, relativePath) |> FullPath.Create
            let! config' = config
            return Ports.run config' (toFsStatement rootFullPath' moduleFullPath' statement innerConfig)
        }


    let interpretDtsModule modulePath content =
        result {
            let statementsResult =
                content
                |> Parser.run

            let! fullModulePath = fullPath modulePath
            let! config' = config
            config'.StatementStore.TryAdd fullModulePath statementsResult |> ignore
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
            let! config' = config
            config'.StatementStore.TryAdd moduleP statementsResult |> ignore
            
            let! statements = statementsResult
                
            let! (_, conf) =
                toFsStatement' testRelativePath statements[0] None

            conf
            |> Option.bind (fun c ->
                c.TryGetLocal (Identifier.create "Foo")
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
            let! config' = config
            config'.StatementStore.TryAdd moduleP statementsResult |> ignore
            
            let! statements = statementsResult
                
            let! (_, conf) =
                toFsStatement' testRelativePath statements[0] None
                |> Result.bind (fun (_, c') ->
                    toFsStatement' testRelativePath statements[1] c'
                )

            conf
            |> Option.bind (fun c ->
                c.TryGetLocal (Identifier.create "FooProps")
            )
            |> shouldL be (ofCase <@ Some @>) "Has no FooProps"

            conf
            |> Option.bind (fun c ->
                c.TryGetLocal (Identifier.create "BarProps")
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
            let! config' = config
            config'.StatementStore.TryAdd moduleP statementsResult |> ignore

            let! statements = statementsResult

            let! fsStatements =
                statements
                |> interpretV2' testRelativePath

            fsStatements 
            |> shouldL haveLength 3 "Wrong count of fs statements"

            let bazProps = fsStatements |> List.last
            bazProps.NestedStatements[0].Kind |> should equal (FsStatementKind.Type FsStatementType.Composition)
            bazProps.NestedStatements[0].NestedStatements |> shouldL haveLength 5 "Wrong NestedStatements count"
            Assert.That(bazProps.NestedStatements[0].NestedStatements, Has.All.Property("Kind").Matches( ofCase <@ FsStatementKind.Field @> ))

            let present = bazProps.ToString()
            present |> should contain "| To of string"
            present |> should contain "| OnSetActive of string -> unit"
        } 
        |> Result.runTest


    [<Test>]
    let ``interpret Button_d_ts like module`` () =
        result {
            let! _ = 
                interpretDtsModule "./Link"
                    """
                        import * as React from 'react';

                        export interface ReactScrollLinkProps {
                            to?: string;
                        }

                        export type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;
                        export default class Link extends React.Component<LinkProps> {}
                    """
                |> Result.bind (interpretV2' "./Link")
                |> Result.ignore

            let! buttonStatements = 
                interpretDtsModule "./Button"
                    """
                        import * as React from 'react';
                        import { ReactScrollLinkProps } from './Link';
                        
                        export type ButtonProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;
                        
                        export default class Button extends React.Component<ButtonProps> {}
                    """

            do ()

            let! fsStatements =
                buttonStatements
                |> interpretV2' "./Button"
                 
            fsStatements 
            |> shouldL haveLength 3 $"Wrong count of fs statements, %A{fsStatements}"

            let buttonProps = fsStatements |> List.item 1
            buttonProps.NestedStatements[0].Kind |> should equal (FsStatementKind.Type FsStatementType.Composition)
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
            let! config' = config
            config'.StatementStore.TryAdd moduleFullPath statementsResultB |> ignore


            let! statements = statementsResultB

            let! fsStatements =
                statements
                |> interpretV2' testRelativePath

            fsStatements 
            |> shouldL haveLength 3 "Wrong count of fs statements"

            fsStatements[0].Kind |> should be (ofCase <@ FsStatementKind.Comment @>)
            fsStatements[0].ToString() |> should contain "// outer lib is not processed yet - import * as Foo from 'foo';"

            let bazProps = fsStatements |> List.last
            bazProps.NestedStatements[0].Kind |> should equal (FsStatementKind.Type FsStatementType.Composition)
            bazProps.NestedStatements[0].NestedStatements |> shouldL haveLength 2 "Wrong NestedStatements count" // Foo.FooProps<FooElement> is unknown
            Assert.That(
                bazProps.NestedStatements[0].NestedStatements, 
                Has.All.Property("Kind").Matches( ofCase <@ FsStatementKind.Field @> ), 
                fun () -> "type alias assertion fails:" )

            let present = bazProps.ToString()
            present |> should contain "/// see also Foo.FooProps<FooElement>"
            present |> should contain "| OnSetActive of string -> unit"
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
            let! config' = config
            config'.StatementStore.TryAdd moduleFullPath statementsResultB |> ignore

            let! statements = statementsResultB

            let! fsStatements =
                statements
                |> interpretV2' testRelativePath

            fsStatements 
            |> shouldL haveLength 4 "Wrong count of fs statements"

            let bar = fsStatements |> List.last
            bar.Open |> should contain "Fable.React"
            bar.Open |> should contain "Fable.Core.JsInterop"
            bar.Scope |> should equal (Scope.Module (ModuleScope.Nested "Bar"))

            let present = bar.ToString()
            // present |> should contain "module Bar ="
            present |> should contain "let inline Bar props children ="
            present |> should contain "domEl (importDefault @\"react-scroll\\modules\\components\\Link\") props children"
        } 
        |> Result.runTest


    [<Test>]
    let ``interface to abstract class interpratation test`` () =
        result {
            let! statements =
                interpretDtsModule "./scroll-events"
                    """
                        interface ScrollEvent {
                            register(eventName: string, callback: (to: string, element: any) => void): void;
                            remove(eventName: string): void;
                        }
                    """

            let! fsStatements =
                statements
                |> interpretV2' "./scroll-events"

            do
                fsStatements
                |> List.iter (fun s -> TestContext.WriteLine($"%O{s}"))
        

            fsStatements
            |> shouldL haveLength 1 $"Wrong count of fs statements"

            let sPresent0 = sprintf "%O" fsStatements[0]
            sPresent0 |> should contain "type ScrollEvent ="
            sPresent0 |> should contain "abstract register : eventName: string -> callback: (string -> obj -> unit) -> unit"
            sPresent0 |> should contain "abstract remove : eventName: string -> unit"
        }
        |> Result.runTest

    [<Test>]
    let ``exported namespace interpretation test like scroll-events`` () =
        result {
            let! statements =
                interpretDtsModule "./scroll-events"
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
                |> Result.map (List.filter FsStatementV2.notHidden)

            do
                fsStatements
                |> List.iter (fun s -> TestContext.WriteLine($"%O{s}"))
            

            fsStatements
            |> shouldL haveLength 3 $"Wrong count of fs statements"

            fsStatements[0].Kind |> should be (ofCase <@ FsStatementKind.AbstractClass @>)
            fsStatements[1].Kind |> should be (ofCase <@ FsStatementKind.AbstractClass @>)
            fsStatements[2].Kind |> should be (ofCase <@ FsStatementKind.LetImportDefault @>)
            fsStatements[2].Open |> should contain "Fable.Core"

            let sPresent0 = sprintf "%O" fsStatements[0]
            sPresent0 |> should contain "type ScrollEvent ="
            sPresent0 |> should contain "abstract register : eventName: string -> callback: (string -> obj -> unit) -> unit"
            sPresent0 |> should contain "abstract remove : eventName: string -> unit"

            let sPresent1 = sprintf "%O" fsStatements[1]
            sPresent1 |> should contain "type Events ="
            sPresent1 |> should contain "abstract registered : obj"
            sPresent1 |> should contain "abstract scrollEvent : ScrollEvent"

            let sPresent2 = sprintf "%O" fsStatements[2]
            sPresent2 |> should contain @"[<ImportDefault(@""react-scroll\scroll-events"")>]"
            sPresent2 |> should contain "let events : Events = jsNative"
        }
        |> Result.runTest


    [<Test>]
    let ``exported function interpretation test`` () =
        result {
            let! statements =
                interpretDtsModule "./animate-scroll"
                    """
                        export function scrollToBottom(options?: any): void;
                    """

            let! fsStatementList =
                interpretV2' "./animate-scroll" statements

            fsStatementList |> should haveLength 1
            TestContext.WriteLine $"%A{fsStatementList.Head}"

            let fsStatement = fsStatementList.Head
            fsStatement.Kind |> should equal (FsStatementKind.LetImport (Identifier "scrollToBottom"))
            
            let presentation = $"%O{fsStatement}"
            presentation |> should contain @"[<Import(""scrollToBottom"", from=@""react-scroll\animate-scroll"")>]"
            presentation |> should contain "let scrollToBottom : options: obj option -> unit = jsNative"
        }
        |> Result.runTest


    [<Test>]
    let ``exported function with annonymous object type patameter interpretation test`` () =
        result {
            let! statements =
                interpretDtsModule "./animate-scroll"
                    """
                        export function getAnimationType(options: { smooth: boolean | string }): (x: number) => number;
                    """

            let! fsStatementList =
                interpretV2' "./animate-scroll" statements

            fsStatementList |> should haveLength 1
            TestContext.WriteLine $"%A{fsStatementList.Head}"

            let fsStatement = fsStatementList.Head
            fsStatement.Kind |> should equal (FsStatementKind.LetImport (Identifier "getAnimationType"))
            
            let presentation = $"%O{fsStatement}"
            presentation |> should contain @"[<Import(""getAnimationType"", from=@""react-scroll\animate-scroll"")>]"
            presentation |> should contain "let getAnimationType : options: {| smooth : U2<bool, string> |} -> (float -> float) = jsNative"
        }
        |> Result.runTest


    [<Test>]
    let ``interface with typeof member interpretation like scroller_d_ts`` () =
        result {
            let! statements =
                interpretDtsModule "./scroller"
                    """
                        export function scrollTo(to: string, props: any): void;

                        export interface Scroller {
                            scrollTo: typeof scrollTo;
                        }
                    """

            let! fsStatementList =
                interpretV2' "./scroller" statements



            fsStatementList |> should haveLength 2
            TestContext.WriteLine $"%A{fsStatementList[0]}"
            TestContext.WriteLine $"%A{fsStatementList[1]}"

            let interfaceFsStatement = fsStatementList[1]
            interfaceFsStatement.Kind |> should equal (FsStatementKind.AbstractClass (Identifier "Scroller"))
            
            let presentation = $"%O{interfaceFsStatement}"
            presentation |> should contain @"type Scroller ="
            presentation |> should contain "abstract scrollTo : to: string -> props: obj -> unit"
        }
        |> Result.runTest


    [<Test>]
    let ``generic type alias with combination against inline object`` () =
        result {
            let! statements =
                interpretDtsModule "./scroll-element"
                    """
                        export type ScrollElementProps<P> = P & {
                            name: string;
                            id?: string | undefined;
                        };
                    """

            let! fsStatements = interpretV2' "./scroll-element" statements

            fsStatements |> should haveLength 1

            let present = fsStatements.Head.ToString()
            present |> should contain "type ScrollElementProps ="
            present |> should contain "| Name of string"
            present |> should contain "| Id of string"
        }
        |> Result.runTest


    [<Test>]
    let ``generic type alias with combination against interface and inline object`` () =
        result {
            do!
                interpretDtsModule "./scroll-element"
                    """
                        export type ScrollElementProps<P> = P & {
                            name: string;
                            id?: string | undefined;
                        };
                    """
                |> Result.bind (interpretV2' "./scroll-element")
                |> Result.ignore

            let! fsStatements =
                interpretDtsModule "./scroll-link"
                    """
                        import { ScrollElementProps } from './scroll-element';

                        export type ScrollLinkProps<P> = ScrollElementProps &
                            P & {
                                container?: HTMLElement | undefined;
                            };
                    """
                |> Result.bind (interpretV2' "./scroll-link")


            fsStatements |> should haveLength 1

            let present = fsStatements.Head.ToString()
            present |> should contain "type ScrollLinkProps ="
            present |> should contain "| Container of HTMLElement"
        }
        |> Result.runTest


    [<Test>]
    let ``collectImportDefault test`` () =
        result {
            let! statements =
                interpretDtsModule "./animate-scroll"
                    """
                        export function scrollToBottom(options?: any): void;
                    """

            let! fsStatementList =
                interpretV2' "./animate-scroll" statements
                |> Result.bind (fun xs ->
                    result {
                        let! rootFullPath' = rootFullPath |> FullPath.Create
                        let! moduleFullPath' = fullPath "./animate-scroll"
                        let! config' = config
                        return Ports.run config' (Facade.collectImportDefault rootFullPath' moduleFullPath' xs)
                    }
                )

            fsStatementList |> should haveLength 2

            fsStatementList[0].Kind |> should equal (FsStatementKind.AbstractClass (Identifier "AnimateScroll"))
            fsStatementList[1].Kind |> should equal ( FsStatementKind.LetImportDefault (Identifier "animateScroll"))

            let presentation0 = $"%O{fsStatementList[0]}"
            presentation0 |> should contain "type AnimateScroll ="
            presentation0 |> should contain "abstract scrollToBottom : options: obj option -> unit"

            let presentation1 = $"%O{fsStatementList[1]}"
            presentation1 |> should contain @"[<ImportDefault(@""react-scroll\animate-scroll"")>]"
            presentation1 |> should contain "let animateScroll : AnimateScroll = jsNative"
        }
        |> Result.runTest




    [<Test>]
    [<Ignore("need to interpret functions")>]
    let ``exported namespace interpretation test like Helpers_d_ts`` () =
        result {
            let! statements =
                interpretDtsModule "./Helpers"
                    """
                        export namespace Helpers {
                            function Scroll(component: any, customScroller?: any): any;
                            function Element(component: any): any;
                        }
                    """

            let! fsStatements =
                statements
                |> interpretV2' "./Helpers"
                |> Result.map (List.filter FsStatementV2.notHidden)

            do
                fsStatements
                |> List.iter (fun s -> TestContext.WriteLine($"%O{s}"))
            

            fsStatements
            |> shouldL haveLength 2 $"Wrong count of fs statements"

            fsStatements[0].Kind |> should be (ofCase <@ FsStatementKind.AbstractClass @>)
            fsStatements[1].Kind |> should be (ofCase <@ FsStatementKind.LetImportDefault @>)
            fsStatements[1].Open |> should contain "Fable.Core"

            let sPresent0 = sprintf "%O" fsStatements[0]
            sPresent0 |> should contain "type Helpers ="
            sPresent0 |> should contain "abstract Scroll : component': any -> customScroller: obj option -> obj"
            sPresent0 |> should contain "abstract Element : component: obj -> obj"

            let sPresent1 = sprintf "%O" fsStatements[1]
            sPresent1 |> should contain "type Events ="
            sPresent1 |> should contain "abstract registered : obj"
            sPresent1 |> should contain "abstract scrollEvent : ScrollEvent"

            let sPresent2 = sprintf "%O" fsStatements[2]
            sPresent2 |> should contain @"[<ImportDefault(""react-scroll\scroll-events"")>]"
            sPresent2 |> should contain "let events : Events = jsNative"
        }
        |> Result.runTest

