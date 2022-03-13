namespace FableTranspiler.Tests

open NUnit.Framework
open FsUnit
open FableTranspiler.Ports
open FableTranspiler.Ports.PortsBuilder

module PortsBuilderTests =

    type TestEnv =
        {
            Element: int
        }

    [<Test>]
    let ``For statement test`` () =
        let mutable coll = [||]

        let action elem =
            ports {
                do
                    coll <- coll |> Array.insertAt 0 elem
            }



        ports {
            for i in 1 .. 10 do action i |> ignore
        }
        |> Ports.run ()

        coll |> should haveLength 10
