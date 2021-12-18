module FableTranspiler.Parsers.Export

open FParsec
open Types
open Common


let exportKeyword = str "export"

let exportStatement =
    exportKeyword >>. ws >>. pchar '=' >>. ws >>. Common.identifier |>> Statement.Export .>> ws .>> pchar ';'