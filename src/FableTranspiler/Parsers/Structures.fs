/// class, interface and type composition
[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Structures

open FParsec
open Types
open Common


let planeType = sepEndBy identifier (ws >>. skipChar '.' >>. ws) |>> TypeName.Plain

let genericType = 
    planeType 
    .>>? skipChar '<' .>> ws .>>. sepEndBy planeType (ws >>. skipChar ',' >>. ws) .>> ws .>> skipChar '>'
    |>> TypeName.Generis

let ``type`` =
    choice [
        planeType
        genericType
    ]


let typeKeyword = skipString "type"

let typeComposition = 
    sepEndBy1 ``type`` (ws >>. skipChar '&' >>. ws) |>> TypeCombination.Composition

let typeUnion = 
    sepEndBy1 ``type`` (ws >>. skipChar '|' >>. ws) |>> TypeCombination.Union

let typeAlias =
    typeKeyword >>. ws1 >>. identifier .>> ws .>> skipChar '=' .>> ws 
        .>>. choice [
            typeComposition
            typeUnion // order make sense
        ]
        |>> TypeAlias