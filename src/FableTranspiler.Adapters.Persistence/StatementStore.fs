module FableTranspiler.Adapters.Persistence.StatementStore

open System.Collections.Concurrent
open System
open FableTranspiler.Parsers.Types
open FableTranspiler.SimpleTypes

let private dict = ConcurrentDictionary<FullPath, Result<StatementList, string>>()

let private tryGetStatementList uri =
    match dict.TryGetValue(uri) with
    | true, statements -> statements |> Some
    | false, _ -> None


let private tryGetStatement uri identifier =
    tryGetStatementList uri
    |> Option.bind (fun result ->
        match result with
        | Ok statements -> statements |> List.tryFind (fun s -> (s |> Statement.name) |> Option.map ((=) identifier) |> Option.defaultValue false)
        | Error _ -> None
    )

let private getOrAdd uri fresult =
    dict.GetOrAdd(uri, new Func<FullPath, Result<StatementList, string>>(fun _ -> fresult ()))


let create () : FableTranspiler.Ports.Persistence.StatementStore =
    {
        ContainsKey = dict.ContainsKey
        TryAdd = fun key value -> dict.TryAdd(key, value)
        TryGetStatement = tryGetStatement
        TryGetStatementList = tryGetStatementList
        GetOrAdd = getOrAdd
    }