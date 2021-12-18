[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Comment

open FParsec
open Types
open Common


let keyword = skipString "//"

let statement : Parser<Statement,unit> =
    ws >>. keyword >>. skipRestOfLine false |>> (fun _ -> Statement.Comment)