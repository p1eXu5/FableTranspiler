[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Export

open FParsec
open Types
open Common


let keyword = skipString "export"

let statement =
    keyword >>. ws >>. skipChar '=' >>. ws >>. Common.identifier |>> Statement.Export .>> ws .>> skipChar ';'