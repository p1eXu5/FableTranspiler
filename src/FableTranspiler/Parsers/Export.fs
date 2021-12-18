module FableTranspiler.Parsers.Export

open FParsec
open Types
open Common


let exportKeyword = skipString "export"

let exportStatement =
    exportKeyword >>. ws >>. skipChar '=' >>. ws >>. Common.identifier |>> Statement.Export .>> ws .>> skipChar ';'