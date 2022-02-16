module FableTranspiler.Domain.UseCases.Implementation


open FableTranspiler.Parsers
open FableTranspiler.Ports.AsyncPortsBuilder
open FableTranspiler.Ports.Persistence


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
            return UriGraph.Root (uri, [])
        | Error err ->
            return UriGraph.ErrorNode (uri, err)
    }