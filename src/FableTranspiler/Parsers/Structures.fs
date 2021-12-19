/// class, interface and type composition
[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Structures

open FParsec
open Types
open Common


let planeType = sepEndBy identifier (ws >>. skipChar '.' >>. ws)
let genericType = planeType .>>? skipChar '<' .>> ws .>>. sepEndBy planeType (ws >>. skipChar ',' >>. ws) .>> ws .>> skipChar '>'

let ``type`` =
    choice [
        planeType
        genericType
    ]


let typeComposition = sepEndBy ``type`` ((ws >>. skipChar '&' >>. ws)
let typeUnion = sepEndBy ``type`` ((ws >>. skipChar '|' >>. ws)