module FableTranspiler.Tests.Common


open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open FableTranspiler.Parsers
open FableTranspiler.Parsers.Types
open FParsec
open System
open FableTranspiler
open System.Diagnostics
open NUnit.Framework.Constraints
open System.Threading.Tasks
open FableTranspiler.SimpleTypes



let inline writeLine s = TestContext.WriteLine(sprintf "%A" s)

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
let shouldSuccess res = function
| Success (s, _, _) -> s |> shouldL equal res ""
| Failure (t, _, _) -> raise (AssertionException($"Should be %A{res} but was %A{t}"))



[<DebuggerStepThrough>]
let inline beOk expected = function
    | Result.Ok ok -> ok |> shouldL equal expected ""
    | Result.Error err -> raise (AssertionException($"Should be %A{expected} but there is an error: %A{err}"))


let modulePath fileName =
    match fileName |> ModulePath.Create with
    | Result.Ok modulePath -> modulePath
    | Result.Error err -> raise (AssertionException err)