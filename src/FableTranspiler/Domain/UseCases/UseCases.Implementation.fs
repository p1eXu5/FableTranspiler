module FableTranspiler.Domain.UseCases.Implementation


open FableTranspiler.Parsers
open FableTranspiler.Ports.AsyncPortsBuilder
open FableTranspiler.Ports.Persistence
open System.IO
open FableTranspiler.SimpleTypes
open FsToolkit.ErrorHandling
open FableTranspiler.Parsers.Types
open System.Threading.Tasks


let parseFile fullPath : AsyncPorts<(StatementStore<Statement> * ReadFileAsync), FullPathTree> =

    let importingFullPaths fullPath : AsyncPorts<(StatementStore<Statement> * ReadFileAsync), FullPath list>  =
        taskPorts {
            let! (store: StatementStore<Statement>, readFileAsync: ReadFileAsync) = AsyncPorts.ask
            use! sr = readFileAsync fullPath
            let! content = sr.ReadToEndAsync()
            let presult = 
                store.GetOrAdd
                    fullPath
                    (fun () -> Parser.run content)

            match presult with
            | Ok statements ->
                let path = fullPath |> FullPath.Value
                return
                    statements
                    |> List.choose (function 
                        | Statement.Import (_, Relative t) ->  t |> Some 
                        | Statement.Export (Transit (_, (Relative t))) ->  t |> Some 
                        | _ -> None
                    )
                    |> List.map (fun (ModulePath modulePath) ->
                        Path.GetFullPath(Path.Combine(Path.GetDirectoryName(path), modulePath + ".d.ts"))
                        |> FullPath.Create
                    )
                    |> List.choose (function Ok p -> Some p | _ -> None) // TODO: add repo for not found paths

            | Error _ ->
                return []
        }

    let rec parseFile' fullPath  =
        taskPorts {
            let! importingFullPaths = importingFullPaths fullPath
            let! (store: StatementStore<Statement>, readFileAsync: ReadFileAsync) = AsyncPorts.ask

            let! nodes =
                importingFullPaths
                |> List.map (fun fp ->
                    AsyncPorts.run (store, readFileAsync) (parseFile' fp)
                )
                |> Task.WhenAll

            let result = FullPathTree.Node (fullPath, nodes |> List.ofArray)
            return result 
        }

    parseFile' fullPath