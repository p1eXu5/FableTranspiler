module FableTranspiler.Infrastruture

open System
open System.IO
open System.Windows
open Microsoft.Win32
open Parsers.Identifier
open FableTranspiler.Parsers.Types
open System.Threading.Tasks


let readFile file =
    task {
        use stream = File.OpenText(file)
        return! stream.ReadToEndAsync()
    }


type StatementsResult =
    {
        Path: string
        Statements: Result<Statements, string>
    }

type ModuleTree =
    | Leaf of StatementsResult
    | Branch of StatementsResult * ModuleTree list


let rec parseFile fileName : Task<ModuleTree> =
    task {
        let! content = readFile fileName
        match document content with
        | Ok statements ->
            let! results =
                statements
                |> List.choose (function 
                    | Statement.Import (_, Relative t) ->  t |> Some 
                    | Statement.Export (Transit (_, (Relative t))) ->  t |> Some 
                    | _ -> None)
                |> List.map (fun (ModulePath m) ->
                    let relativePath = Path.Combine(Path.GetDirectoryName(fileName), m + ".d.ts")
                    parseFile relativePath
                )
                |> Task.WhenAll

            return 
                (
                    {
                        Path = fileName
                        Statements = (statements |> Ok)
                    }
                    , results |> Array.toList
                ) 
                |> Branch

        | Error err -> 
            return
                {
                    Path = fileName
                    Statements = Error err
                }
                |> Leaf
    }

let openFile () =
    task {
        let fd = OpenFileDialog()
        match fd.ShowDialog() |> Option.ofNullable with
        | Some _ ->
            let! tree = parseFile fd.FileName
            return tree |> Ok
        | None -> return Error "open file canceled"
    }