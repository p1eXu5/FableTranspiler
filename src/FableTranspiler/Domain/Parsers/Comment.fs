[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Comment

open FParsec
open Types
open Common


let keyword = skipString "//"

let statement : Parser<Statement,unit> =
    ws >>? keyword >>. manySatisfy (function '\n' -> false | _ -> true) |>> (fun comment -> Statement.Comment ("//" + comment))