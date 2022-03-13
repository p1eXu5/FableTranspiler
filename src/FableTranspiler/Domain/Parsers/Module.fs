[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Module

open FableTranspiler.SimpleTypes
open FParsec
open Types
open Common
open System


let modulePath =
    skipQuote
    >>? many1CharsTill anyChar skipQuote
    |>> (fun path -> ModulePath (path.Trim()))

let relative = 
    skipQuote 
    >>? (pchar '.') 
    .>>.? many1CharsTill anyChar skipQuote 
    |>> (fun path -> ModulePath ((sprintf "%c%s" (fst path) (snd path))) |> Relative)

let nodeModuleTilda =
    skipQuote 
    >>? (noneOf ['.'; '/'; '\\'] <|> satisfy (fun ch -> ch = '~' || Char.IsLetterOrDigit(ch)))
    .>>.? many1CharsTill anyChar skipQuote 
    |>> (fun path -> ModulePath ((sprintf "%c%s" (fst path) (snd path))) |> NodeModule)

let nodeModuleFolder =
    skipQuote 
    >>? pipe2 (pstring "node_modules") (anyOf ['/'; '\\']) (sprintf "%s%c")
    .>>.? many1CharsTill anyChar skipQuote 
    |>> (fun (p1, p2) -> ModulePath ((sprintf "%s%s" (p1) (p1))) |> NodeModule)

let node =
    choice [
        nodeModuleTilda
        nodeModuleFolder
    ]

let dtsModule =
    choice [
        relative
        node
    ]