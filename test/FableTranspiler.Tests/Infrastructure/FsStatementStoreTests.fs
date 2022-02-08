namespace FableTranspiler.Tests.Infrastructure

open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types
open FableTranspiler.VmAdapters.FsInterpreter.Types
open FableTranspiler.Infrastruture
open NUnit.Framework
open FableTranspiler.Tests.Common
open FsUnit

module FsStatementStoreTests =

    let private store = FsStatementInMemoryStore.store


    [<Test>]
    let ``WHEN store has statement THEN statments can be obtained through identifier and quilifier`` () =
        let identifier = Identifier "foo"

        let testStatement =
            FsStatement.Typed (identifier, [], [])

        let modulePath = modulePath @"z:\bar\baz.d.ts"

        do
            store.Add modulePath testStatement

        let reader = store.FsStatementReader ()

        reader [identifier] |> shouldL equal testStatement ""
        reader [Identifier "baz"; identifier] |> shouldL equal testStatement ""