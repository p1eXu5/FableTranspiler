namespace FableTranspiler.Tests.Parsers

open NUnit.Framework
open System.Threading.Tasks

[<Category("Statements")>]
module Statements =

    open FsUnit
    open FsCheck
    open FsCheck.NUnit

    open FableTranspiler.Parsers
    open FableTranspiler.Parsers.Types
    open FableTranspiler.Parsers.Identifier
    open FParsec
    open System
    open FableTranspiler
    open System.Diagnostics
    open NUnit.Framework.Constraints
    open FableTranspiler.Tests.Parsers.Common




    type IdentifierInput = IdentifierInput of string with
        static member op_Explicit(IdentifierInput i) = i

    type ArbitraryModifiers =
        static member IdentifierInput() =
            let xs = 
                seq { 
                    for ch in 'a' .. 'c' do
                        yield ch
                    yield '_'
                }

            Gen.elements xs
            |> Gen.listOfLength 10
            |> Gen.map (Array.ofList >> String)
            |> Arb.fromGen
            |> Arb.convert IdentifierInput string




    [<OneTimeSetUp>]
    let registerArbitraryModifiers () =
        Arb.register<ArbitraryModifiers> ()
        |> ignore



    let [<Literal>] node_mudules = __SOURCE_DIRECTORY__ + @"\..\..\..\node_modules"
    let [<Literal>] ``types/reac-scroll/index.d.ts`` = node_mudules + @"\@types\react-scroll\index.d.ts"
    let [<Literal>] ``types/reac-scroll/modules/index.d.ts`` = node_mudules + @"\@types\react-scroll\modules\index.d.ts"

    open FSharp.Control
    open System.IO

    let readFile file =
        task {
            use stream = File.OpenText(file)
            return! stream.ReadToEndAsync()
        }


    [<Test>]
    [<Category("Statements")>]
    let ``test react-scroll index_d_ts parsing`` () : Task =
        task {
            let! input = readFile ``types/reac-scroll/index.d.ts``
            let doc = document input
            let expected : StatementList =
                [
                    Dsl.Comment.create "// Type definitions for react-scroll 1.8"
                    Dsl.Comment.create "// Project: https://github.com/fisshy/react-scroll"
                    Dsl.Comment.create "// Definitions by: Ioannis Kokkinidis <https://github.com/sudoplz>"
                    Dsl.Comment.create "//                 Giedrius Grabauskas <https://github.com/GiedriusGrabauskas>"
                    Dsl.Comment.create "// Definitions: https://github.com/DefinitelyTyped/DefinitelyTyped"
                    Dsl.Comment.create "// TypeScript Version: 2.8"
                    Dsl.Import.allAliased "ReactScroll" "./modules/index"
                    Dsl.Export.outAssignment "ReactScroll"
                 ]
            return
                doc |> beOk expected
        }

    [<Test>]
    [<Category("Statements")>]
    let ``test react-scroll modules index_d_ts parsing`` () : Task =
        task {
            let! input = readFile ``types/reac-scroll/modules/index.d.ts``
            let doc = document input
            let expeced : StatementList =
                [
                    Dsl.Export.defaultAliasedS "Button" "./components/Button"
                    Dsl.Export.defaultAliasedS "Element" "./components/Element"
                    Dsl.Export.transit ["Link" |> Choice1Of2; "LinkProps" |> Choice2Of2] "./components/Link"
                    Dsl.Export.namedS "Helpers" "./mixins/Helpers"
                    Dsl.Export.defaultAliasedS "Events" "./mixins/scroll-events"
                    Dsl.Export.transit ["ScrollElement" |> Choice1Of2; "ScrollElementProps" |> Choice2Of2] "./mixins/scroll-element"
                    Dsl.Export.transit ["ScrollLink" |> Choice1Of2; "ScrollLinkProps" |> Choice2Of2] "./mixins/scroll-link"
                    Dsl.Export.defaultAliasedS "scrollSpy" "./mixins/scroll-spy"
                    Dsl.Import.allAliased "scroller" "./mixins/scroller"
                    Dsl.Import.allAliased "animateScroll" "./mixins/animate-scroll"
                    Dsl.Export.outList ["animateScroll"; "scroller"]
                 ]
            return
                doc |> beOk expeced
        }



    [<Property>]
    let ``identifier tests`` (IdentifierInput i) =
        let d = run Identifier.identifier i
        d
        |> function
            | ParserResult.Success (_,_,_) -> true
            | _ -> false


    [<Test>]
    [<Ignore("add after")>]
    let ``const object expression test`` () =

        let input = """
            const mimeTypes = Object.freeze({
              allFiles: '*/*',
              audio: 'audio/*',
              csv: 'text/csv',
            } as const)
        """

        let output =
            [
                Statement.Const (
                    Expression.Binary (
                        ExpressionKind.Assignment
                        , Expression.Identifier ("mimeTypes" |> Identifier.Create)
                        , Expression.Binary ( 
                            ExpressionKind.Dereferentiation
                            , Expression.Identifier ("Object" |> Identifier.Create)
                            , Expression.Function (
                                "freeze"
                                , Expression.Binary (
                                    ExpressionKind.AsCast
                                    , Expression.ObjectLiteral [
                                        Expression.Binary (ExpressionKind.Typification, Expression.Identifier ("allFiles" |> Identifier.Create), Expression.StringLiteral ("*/*" |> StringLiteral.Create)) 
                                        Expression.Binary (ExpressionKind.Typification, Expression.Identifier ("audio" |> Identifier.Create), Expression.StringLiteral ("audio/*" |> StringLiteral.Create)) 
                                        Expression.Binary (ExpressionKind.Typification, Expression.Identifier ("csv" |> Identifier.Create), Expression.StringLiteral ("text/csv" |> StringLiteral.Create)) 
                                    ]
                                    , Expression.Identifier ("const" |> Identifier.Create)
                                )
                            )
                        )
                    )
                )
            ]

        let a = StringLiteral "sdf"
        let b = StringLiteral "sdf"

        document input |> shouldL equal (Result<StatementList, string>.Ok output)


    [<Test>]
    [<Ignore("add after")>]
    let ``const expression with dereferencing member`` () =

        let input = """
            const mimeTypes = Object.freeze
        """

        let expected : Result<StatementList, string> =
            Statement.Const (
                Expression.Binary (
                    ExpressionKind.Assignment
                    , Expression.Identifier ("mimeTypes"  |> Identifier.Create)
                    , Expression.Binary (
                        ExpressionKind.Dereferentiation
                        ,Expression.Identifier ("Object"  |> Identifier.Create) 
                        ,Expression.Identifier ("freeze"  |> Identifier.Create))
                )
            )
            |> List.replicate 1
            |> Result.Ok


        let actual = 
            document input

        Assert.That( actual, Is.EqualTo(expected), $"Actual: %A{actual}")


    [<Test>]
    [<Ignore("add after")>]
    let ``const expression with dereferencing member invokation`` () =

        let input = """
            const mimeTypes = Object.freeze()
        """

        let expected : Result<StatementList, string> =
            Statement.Const (
                Expression.Binary (
                    ExpressionKind.Assignment
                    , Expression.Identifier ("mimeTypes"  |> Identifier.Create)
                    , Expression.Binary (
                        ExpressionKind.Dereferentiation
                        , Expression.Identifier ("Object" |> Identifier.Create)
                        , Expression.Function ("freeze", Expression.Empty))
                )
            )
            |> List.replicate 1
            |> Result.Ok


        let actual = 
            document input

        Assert.That( actual, Is.EqualTo(expected), $"Actual: %A{actual}")


