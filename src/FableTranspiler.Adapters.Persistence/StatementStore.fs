module FableTranspiler.Adapters.Persistence.StatementStore

open System.Collections.Concurrent
open System
open FableTranspiler.Parsers.Types
open FableTranspiler.SimpleTypes

let private dict = ConcurrentDictionary<FullPath, StatementList>()

let private tryGetValue uri identifier =
    match dict.TryGetValue(uri) with
    | true, statements -> statements |> List.tryFind (fun s -> (s |> Statement.name) |> Option.map ((=) identifier) |> Option.defaultValue false)
    | false, _ -> None

let create () : FableTranspiler.Ports.Persistence.StatementStore =
    {
        ContainsKey = dict.ContainsKey
        TryAdd = fun key value -> dict.TryAdd(key, value)
        TryGetValue = tryGetValue
    }