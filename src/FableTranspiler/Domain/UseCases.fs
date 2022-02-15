module FableTranspiler.Domain.UseCases

open System
open FableTranspiler.Parsers
open FableTranspiler.Ports.AsyncPortsBuilder
open FableTranspiler.Ports.Persistence

type UriGraph =
    | Node of Node: Uri * Importing: UriGraph list * Imported: UriGraph list
    | ErrorNode of Node: Uri * Error: string

type ParseFileUseCase = Uri -> UriGraph


let parseFile uri : AsyncPorts<(StatementStore * ReadFileAsync), UriGraph> =
    taskPorts {
        let! (store: StatementStore, readFileAsync: ReadFileAsync) = AsyncPorts.ask
        use! sr = readFileAsync uri
        let! content = sr.ReadToEndAsync()

        match Parser.run content with
        | Ok statements ->
            do
                store.TryAdd uri statements
                |> ignore
            return UriGraph.Node (uri, [], [])
        | Error err ->
            return UriGraph.ErrorNode (uri, err)
    }