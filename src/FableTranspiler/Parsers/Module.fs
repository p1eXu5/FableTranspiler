[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Module

open FParsec
open Types
open Common
open System

let relative = 
    quote 
    >>? (pchar '.') 
    .>>.? many1CharsTill anyChar quote 
    |>> (fun path -> ModulePath ((sprintf "%c%s" (fst path) (snd path))) |> Relative)

let node = 
    quote 
    >>? (noneOf ['.'; '/'; '\\'] <|> satisfy (fun ch -> ch = '~' || Char.IsLetterOrDigit(ch)))
    .>>.? many1CharsTill anyChar quote 
    |>> (fun path -> ModulePath ((sprintf "%c%s" (fst path) (snd path))) |> NodeModule)

