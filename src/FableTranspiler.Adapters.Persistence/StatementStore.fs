module FableTranspiler.Adapters.Persistence.StatementStore

open System.Collections.Concurrent
open System
open FableTranspiler.Parsers.Types

let private dict = ConcurrentDictionary<Uri, StatementList>()

let create () : FableTranspiler.Ports.Persistence.StatementStore =
    {
        ContainsKey = dict.ContainsKey
        TryAdd = fun key value -> dict.TryAdd(key, value)
    }