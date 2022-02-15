module FableTranspiler.Tests.ParseFileSteps

open NUnit.Framework
open FsUnit
open TickSpec
open FableTranspiler.Domain.UseCases
open System
open System.IO
open System.Text
open FableTranspiler.Ports.Persistence
open FableTranspiler.Ports.AsyncPortsBuilder
open FableTranspiler.Adapters.Persistence

type TestConfig =
    {
        Uri : Uri
        StatementStore : StatementStore
        ReadFileAsync: ReadFileAsync
    }

let [<Given>] ``a "(.*)" file with content:`` (path: string) (content: string) =
    {
        Uri = Uri(path)
        StatementStore = StatementStore.create ()
        ReadFileAsync =
            fun _ ->
                task {
                    let bytes = Encoding.UTF8.GetBytes(content)
                    return new StreamReader(new MemoryStream(bytes))
                }
    }

let [<When>] ``the file is parsing`` (config: TestConfig) =
    parseFile config.Uri
    |> AsyncPorts.run (config.StatementStore, config.ReadFileAsync)
    , config

let [<Then>] ``an tree with single node is produced`` (graph: UriGraph, config: TestConfig) =
    graph |> should be (ofCase <@ UriGraph.Node (config.Uri, [], []) @>)

let [<Then>] ``the (.*) statement is stored`` (name: string) (graph: UriGraph, config: TestConfig) =
    ()