module FableTranspiler.Infrastruture

open System
open System.IO
open System.Windows
open Microsoft.Win32
open Parsers.Identifier

let readFile file =
    task {
        use stream = File.OpenText(file)
        return! stream.ReadToEndAsync()
    }

let parseFile fileName =
    task {
        let! content = readFile fileName
        return document content
    }

let openFile () =
    task {
        let fd = OpenFileDialog()
        match fd.ShowDialog() |> Option.ofNullable with
        | Some _ -> return! parseFile fd.FileName
        | None -> return Error ""
    }