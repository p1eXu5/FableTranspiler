namespace FableTranspiler.Tests.BDD

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
open FableTranspiler.Tests.BDD
open FableTranspiler.SimpleTypes
open FsToolkit.ErrorHandling
open FableTranspiler.Domain.UseCases.Implementation
open FableTranspiler.Tests.Common

type ParseFileFeatureFixture() =
    inherit FeatureFixture()
    [<TestCaseSource("Scenarios")>]
    override _.Bdd(scenario: Scenario) = base.Bdd(scenario)
    static member Scenarios = FeatureFixture.GenerateTestCaseData("ParseFile")


type TestConfig =
    {
        StatementStore : StatementStore
        ReadFileAsync: ReadFileAsync
    }

module ParseFileSteps =

    let initReadFileAsync : ReadFileAsync =
        fun _ ->
            task {
                return raise (FileNotFoundException())
            }


    let [<Given>] ``a (.*) file with content:`` (path: string) (content: string) =
        result {
            let! uri = path |> FullPath.Create
            return {
                StatementStore = StatementStore.create ()
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
                    StatementStore = StatementStore.create ()
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
                , config
            return foo
        }


    let [<Then>] ``a tree with single (.*) root node is produced`` (path: string) (actionResult: Result<(UriGraph * TestConfig), string>) =
        result {
            let! (graph,_) = actionResult
            let! fullPath = path |> FullPath.Create
            do
                graph |> should be (ofCase <@ UriGraph.Root (fullPath, []) @>)
        }


    let [<Then>] ``the (.*) statement from (.*) file is stored`` (name: string) (path: string) (actionResult: Result<(UriGraph * TestConfig), string>) =
        result {
            let! (_,config) = actionResult

            let! uri = path |> FullPath.Create
            config.StatementStore.ContainsKey uri |> should equal true
            config.StatementStore.TryGetValue uri (Identifier name) |> should be (ofCase <@ Some @>)
        }


    let [<Then>] ``a tree has (.*) root node importing:`` (path: string) (importingPathes: string[]) (actionResult: Result<(UriGraph * TestConfig), string>) =
        result {
            let! (graph,_) = actionResult
            let! uri = path |> FullPath.Create
            graph |> UriGraph.uri |> should equal uri

            let! expectedImportingUriList = importingPathes |> List.ofArray |> List.traverseResultM (FullPath.Create)
            let actualImportingUriList = graph |> (function | UriGraph.ImportingUriList l -> l | _ -> [])
            actualImportingUriList |> shouldL equivalent expectedImportingUriList "Expected importing uri list for root node is wrong."
        }


    let [<Then>] ``a (.*) node is imported to:`` (path: string) (importingPathes: string[]) (actionResult: Result<(UriGraph * TestConfig), string>) =
        result {
            let! (graph,_) = actionResult
            let! uri = path |> FullPath.Create
            match UriGraph.tryFind uri 0 graph with
            | Some node -> 
                let! expectedImportedToList = importingPathes |> List.ofArray |> List.traverseResultM (FullPath.Create)
                let actualImportedToList = node |> (function | UriGraph.ImportedToUriList l -> l | _ -> [])
                do
                    actualImportedToList |> should equivalent expectedImportedToList
            | _ -> return raise (AssertionException($"There is not an {uri} in graph."))
        }