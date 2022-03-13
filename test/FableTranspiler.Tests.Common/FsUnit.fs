﻿namespace FableTranspiler.Tests.Common

open NUnit.Framework
open FsUnit
open FsCheck
open FParsec
open System.Diagnostics
open NUnit.Framework.Constraints
open System.Threading.Tasks
open FableTranspiler.SimpleTypes


module FsUnit =


    let inline writeLine s = TestContext.WriteLine(sprintf "%A" s)
    let inline writeLineS (s: string) = TestContext.WriteLine(s)

    let toTask computation : Task = Async.StartAsTask computation :> _


    [<DebuggerNonUserCode>]
    let shouldL (f: 'a -> #Constraint) x message (y: obj) =
        let c = f x

        let y =
            match y with
            | :? (unit -> unit) -> box(TestDelegate(y :?> unit -> unit))
            | _ -> y

        if isNull(box c) 
            then Assert.That(y, Is.Null) 
            else
                let divider = String.replicate 40 "-"
                Assert.That(y, c, fun _ -> sprintf "%s\n  %s" message divider)



    [<DebuggerStepThrough>]
    let shouldSuccessEqual res = function
    | Success (s, _, _) -> s |> shouldL equal res ""
    | Failure (t, _, _) -> raise (AssertionException($"Should be %A{res} but was %A{t}"))


    [<DebuggerStepThrough>]
    let shouldSuccess ``constraint`` res = function
    | Success (s, _, _) -> s |> should ``constraint`` res
    | Failure (t, _, _) -> raise (AssertionException($"Should be %A{res} but was %A{t}"))



    [<DebuggerStepThrough>]
    let inline shouldEqual expected = function
        | Result.Ok ok -> ok |> shouldL equal expected ""
        | Result.Error err -> raise (AssertionException($"Should be %A{expected} but there is an error: %A{err}"))

    [<DebuggerStepThrough>]
    let inline shouldBe expected = function
        | Result.Ok ok -> ok |> shouldL be expected ""
        | Result.Error err -> raise (AssertionException($"Should be %A{expected} but there is an error: %A{err}"))

    module Result =

        let runTest = function
            | Result.Ok _ -> ()
            | Result.Error err -> raise (AssertionException(err))
