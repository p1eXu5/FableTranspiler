module FableTranspiler.Adapters.Persistence.StatementStore

open System.Collections.Concurrent
open System
open FableTranspiler.SimpleTypes


let create<'TStatement> getIdentifier : FableTranspiler.Ports.Persistence.StatementStore<'TStatement> =

    let dict = ConcurrentDictionary<FullPath, Result<'TStatement list, string>>()
    let tryGetStatementList uri =
        match dict.TryGetValue(uri) with
        | true, statements -> statements |> Some
        | false, _ -> None


    let tryGetStatement uri identifier =
        tryGetStatementList uri
        |> Option.bind (fun result ->
            match result with
            | Ok statements -> statements |> List.tryFind (fun s -> (s |> getIdentifier) |> Option.map ((=) identifier) |> Option.defaultValue false)
            | Error _ -> None
        )

    let getOrAdd uri fresult =
        dict.GetOrAdd(uri, new Func<FullPath, Result<'TStatement list, string>>(fun _ -> fresult ()))

    let addOrUpdate uri fresult =
        dict.AddOrUpdate(
            uri, 
                new Func<FullPath, Result<'TStatement list, string>>(fun _ -> fresult), 
                new Func<FullPath, Result<'TStatement list, string>, Result<'TStatement list, string>>(fun _ _ -> fresult)
        )

    {
        ContainsKey = dict.ContainsKey
        TryAdd = fun key value -> dict.TryAdd(key, value)
        TryGetStatement = tryGetStatement
        TryGetStatementList = tryGetStatementList
        GetOrAdd = getOrAdd
        AddOrUpdate = addOrUpdate
    }