[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Export

open FParsec
open Types
open Common


let exportKeyword = skipString "export"

let defaultKeyword = skipString "default"

let defaultAliased = 
    defaultKeyword 
        >>. ws1
        >>? (skipString "as" <?> "expected 'as' in default aliased export")
        >>. ws1
        >>. identifier 
        |>> ExportEntity.DefaultAliased


let namedEntity =
    notFollowedByL (defaultKeyword) "its not named export entity"
    >>? Common.identifier .>>? notFollowedByL (str " as") "named entity must not followed by 'as'"



let outAssignment =
    exportKeyword >>. ws >>? skipChar '=' >>. ws >>. Common.identifier |>> ExportStatement.OutAssignment .>> ws .>> skipChar ';'

let outList =
    exportKeyword
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
    exportKeyword
    >>. ws
    >>? choice [
        openBrace
            >>? sepBy1 (attempt(ws >>. entity .>> ws)) (skipChar ',') 
            .>> ws 
            .>> closedBrace
        entity |>> List.singleton
    ]
    .>> ws1 
    .>>? (skipString "from " <?> "expected 'from' keyword in transit export statement")
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
        exportKeyword >>. ws1 >>? Structures.typeAlias |>> ExportStatement.Structure
        exportKeyword >>. ws1 >>? Structures.interfaceDefinition |>> ExportStatement.Structure
        exportKeyword >>. ws1 >>? defaultKeyword >>. ws1 >>? Structures.classDefinition |>> ExportStatement.Structure
        exportKeyword >>. ws1 >>? Structures.functionDefnition |>> ExportStatement.Structure
    ]
    |>> Statement.Export