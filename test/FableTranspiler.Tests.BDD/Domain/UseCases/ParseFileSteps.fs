namespace rec FableTranspiler.Tests.BDD

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
open FableTranspiler.Tests.BDD.Base
open FableTranspiler.SimpleTypes
open FsToolkit.ErrorHandling
open FableTranspiler.Domain.UseCases.Implementation
open FableTranspiler.Tests.Common.FsUnit
open FableTranspiler.Parsers.Types

[<TestFixture(TestName="Feature: Parse file")>]
type ParseFileFeature() = 
    inherit FeatureFixture()

    [<TestCaseSource("Scenarios")>]
    override _.Bdd(scenario: Scenario) = base.Bdd(scenario)
    static member Scenarios = 
        FeatureFixture.GenerateTestCaseData(
            "ParseFile",
            nameof ParseFileSteps,
            nameof CommonSteps
        )


type TestConfig =
    {
        StatementStore : StatementStore<Statement>
        ReadFileAsync: ReadFileAsync
    }

module ParseFileSteps =

    let [<Given>] ``a (.*) file with content:`` (path: string) (content: string) =
        result {
            let! uri = path |> FullPath.Create
            return {
                StatementStore = StatementStore.create (Statement.identifier)
                ReadFileAsync =
                    fun uri' ->
                        task {
                            if uri <> uri' then
                                return raise (FileNotFoundException())
                            else
                                let bytes = Encoding.UTF8.GetBytes(content)
                                return new StreamReader(new MemoryStream(bytes))
                        }
            }
        }
    
    let [<Given>] ``an another (.*) file with content:`` (path: string) (content: string) (configResult: Result<TestConfig, string>) =
        result {
            let! config = configResult
            let! uri = path |> FullPath.Create
            return {
                config with
                    StatementStore = StatementStore.create (Statement.identifier)
                    ReadFileAsync =
                        fun uri' ->
                            task {
                                if uri = uri' then
                                    let bytes = Encoding.UTF8.GetBytes(content)
                                    return new StreamReader(new MemoryStream(bytes))
                                else
                                    return! config.ReadFileAsync uri'
                            }
            }
        }


    let [<When>] ``the (.*) file is parsing`` (path: string) (configResult: Result<TestConfig, string>) =
        result {
            let! config = configResult
            let! fullPath = path |> FullPath.Create
            let foo =
                parseFile (fullPath)
                |> AsyncPorts.run (config.StatementStore, config.ReadFileAsync)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            return (foo, config)
        }


    let [<Then>] ``a tree with single (.*) node is produced`` (path: string) (actionResult: Result<(FullPathTree * TestConfig), string>) =
        result {
            let! (graph,_) = actionResult
            let! fullPath = path |> FullPath.Create
            do
                graph |> should be (ofCase <@ FullPathTree.Node (fullPath, []) @>)
        }


    let [<Then>] ``the (.*) statement from (.*) file is stored`` (name: string) (path: string) (actionResult: Result<(FullPathTree * TestConfig), string>) =
        result {
            let! (_,config) = actionResult

            let! uri = path |> FullPath.Create
            config.StatementStore.ContainsKey uri |> should equal true
            config.StatementStore.TryGetStatement uri (Identifier name) |> should be (ofCase <@ Some @>)
        }


    let [<Then>] ``a tree (.*) node is importing:`` (path: string) (importingPathes: string[]) (actionResult: Result<(FullPathTree * TestConfig), string>) =
        result {
            let! (graph,_) = actionResult
            let! uri = path |> FullPath.Create
            graph |> FullPathTree.fullPath |> should equal uri

            let! expectedImportingUriList = importingPathes |> List.ofArray |> List.traverseResultM (FullPath.Create)
            let actualImportingUriList = graph |> (function | FullPathTree.ImportingUriList l -> l | _ -> [])
            actualImportingUriList |> shouldL equivalent expectedImportingUriList "Expected importing uri list for root node is wrong."
        }


    let [<Then>] ``a (.*) node is importing nothing`` (path: string) (actionResult: Result<(FullPathTree * TestConfig), string>) =
        result {
            let! (graph,_) = actionResult
            let! uri = path |> FullPath.Create
            match FullPathTree.tryFind uri 0 graph with
            | Some node -> 
                node |> FullPathTree.importingList |> should be Empty
            | _ -> return raise (AssertionException($"There is not an {uri} in graph."))
        }