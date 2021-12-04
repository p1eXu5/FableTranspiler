module FableTranspiler.Tests.ParserTests

open NUnit.Framework
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


[<DebuggerStepThrough>]
let beOk expected = function
    | Result.Ok ok -> Assert.That( ok, FsUnit.Equality.IsEqualTo(expected))
    | Result.Error err -> failwithf "There is an error: %A" err



let inline writeLine s = TestContext.WriteLine(sprintf "%A" s)

[<DebuggerNonUserCode>]
let shouldL (f: 'a -> #Constraint) x (y: obj) =
    let c = f x

    let y =
        match y with
        | :? (unit -> unit) -> box(TestDelegate(y :?> unit -> unit))
        | _ -> y

    if isNull(box c) 
    then Assert.That(y, Is.Null) 
    else Assert.That(y, c, fun () -> sprintf "%A" y)



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

open FSharp.Control
open System.IO

let readFile file =
    task {
        return! File.ReadAllLinesAsync(file)
    }



// #region import parser tests
open FableTranspiler.Tests.ParsersTests

[<TestCaseSource(typeof<TestCases>, nameof TestCases.ImportCases)>]
let ``import statements - module name test`` (content: string) =
    run Import.importStatement content



// #endregion

[<Property>]
let ``identifier tests`` (IdentifierInput i) =
    let d = run Identifier.identifier i
    d
    |> function
        | ParserResult.Success (_,_,_) -> true
        | _ -> false


[<Test>]
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

    document input |> shouldL equal (Result<Statements, string>.Ok output)


[<Test>]
let ``const expression with dereferencing member`` () =

    let input = """
        const mimeTypes = Object.freeze
    """

    let expected : Result<Statements, string> =
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
let ``const expression with dereferencing member invokation`` () =

    let input = """
        const mimeTypes = Object.freeze()
    """

    let expected : Result<Statements, string> =
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