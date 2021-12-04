module FableTranspiler.Tests.FsCheckTests

open NUnit.Framework
open FsUnit
open FsCheck

let inline writeLine s = TestContext.WriteLine(sprintf "%A" s)


type IdentifierInput = IdentifierInput of string with
    static member op_Explicit(IdentifierInput i) = i



[<Test>]
let ``string with no whitespaces`` () =

    let g = Arb.Default.NonWhiteSpaceString().Generator

    g
    |> Gen.sample 10 1
    |> writeLine

    let xs = 
        seq { 
            for ch in 'a' .. 'c' do
                yield ch
            yield '_'
        }

    let arb = 
        Gen.elements xs
        |> Gen.listOfLength 10
        |> Gen.map (Array.ofList >> System.String)
        |> Arb.fromGen
        |> Arb.convert IdentifierInput string



    Arb.toGen arb
    |> Gen.sample 0 10
    |> writeLine