module FableTranspiler.Infrastruture

open System.IO
open Microsoft.Win32
open Parsers.Identifier
open FableTranspiler.Parsers.Types
open System.Threading.Tasks


type ParsingResult =
    {
        Path: string
        Statements: Result<StatementList, string>
    }

type ModuleTree =
    | Leaf of ParsingResult
    | Branch of ParsingResult * ModuleTree list


let readFile file =
    task {
        use stream = File.OpenText(file)
        return! stream.ReadToEndAsync()
    }


let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])


let rec parseFile fileName (accum: Map<string, ModuleTree>) : Task<(ModuleTree * Map<string, ModuleTree>)> =
    task {
        match accum.TryGetValue fileName with
        | true, tree -> return tree, accum
        | false, _ ->
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
                        parseFile relativePath accum
                    )
                    |> Task.WhenAll


                let statementResult =
                    {
                        Path = fileName
                        Statements = (statements |> Ok)
                    }

                return 
                    if results.Length > 0 then

                        let accum' =
                            results
                            |> Array.map snd
                            |> Array.reduce join

                        let trees = 
                            results
                            |> Array.map fst

                        (statementResult, trees |> Array.toList) 
                        |> Branch
                        , accum'
                    else
                        statementResult |> Leaf
                        , accum

            | Error err -> 
                let tree =
                    {
                        Path = fileName
                        Statements = Error err
                    }
                    |> Leaf

                return tree, (accum |> Map.add fileName tree)

    }

let openFile () =
    task {
        let fd = OpenFileDialog()
        fd.Filter <- "d.ts files (*.d.ts)|*.d.ts|All files (*.*)|*.*"
        match fd.ShowDialog() |> Option.ofNullable with
        | Some _ ->
            let! tree = parseFile fd.FileName Map.empty
            return fst tree |> Ok
        | None -> return Error "open file canceled"
    }