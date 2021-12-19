﻿[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Export

open FParsec
open Types
open Common


let keyword = skipString "export"

let ``default`` = skipString "default"

let defaultAliased = 
    ``default`` 
        >>. ws1
        >>. skipString "as"
        >>. ws1
        >>. identifier 
        |>> ExportEntity.DefaultAliased


let namedEntity =
    notFollowedByL (``default``) "its not named export entity"
    >>? Common.identifier .>>? notFollowedByL (str " as") "named entity must not followed by 'as'"



let outAssignment =
    keyword >>. ws >>? skipChar '=' >>. ws >>. Common.identifier |>> ExportStatement.OutAssignment .>> ws .>> skipChar ';'

let outList =
    keyword
        >>. ws1
        >>? openBrace 
        >>? ws 
        >>? sepEndBy1 (ws >>. namedEntity .>> ws) (skipChar ',') 
        .>> ws 
        .>> closedBrace
        .>> ws
        .>>? notFollowedByL (str "from") "unexpected 'from' keyword in out list export"
        .>> skipChar ';'
        |>> OutList


let entity =
    choice [
        namedEntity |>> ExportEntity.Named
        defaultAliased
    ]


let transit =
    keyword
    >>. ws
    >>? choice [
        openBrace
            >>? sepEndBy1 (ws >>. entity .>> ws) (skipChar ',') 
            .>> ws 
            .>> closedBrace
        entity |>> List.singleton
    ]
    .>> ws1 
    .>> skipString "from "
    .>>. choice [
        Module.relative
        Module.node
    ]
    |>> ExportStatement.Transit
    .>> ws
    .>> skipChar ';'


let statement =
    ws
    >>? choice [
        outAssignment
        outList
        transit
    ]
    |>> Statement.Export