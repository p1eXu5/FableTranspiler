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


let outDefault =
    exportKeyword
    >>? ws1
    >>? defaultKeyword
    >>. ws1
    >>? notFollowedByString "class"
    >>? notFollowedByString "function"
    >>. identifier
    |>> OutDefault
    .>> (skipChar ';' <?> "expected ';' out default terminator")


let statement =
    ws
    >>? choice [
        outDefault <?> "out default error"
        outAssignment <?> "out alias error"
        outList <?> "out list error"
        transit <?> "out transit error"
        exportKeyword >>. ws1 >>? defaultKeyword >>. ws1 >>. Structures.statement |>> ExportStatement.StructureDefault
        exportKeyword >>. ws1 >>? Structures.statement |>> ExportStatement.Structure
        //exportKeyword >>. ws1 >>? Structures.plainTypeAlias |>> ExportStatement.Structure
        //exportKeyword >>. ws1 >>? Structures.interfaceDefinition |>> ExportStatement.Structure
        //exportKeyword >>. ws1 >>? defaultKeyword >>. ws1 >>? Structures.classDefinition |>> ExportStatement.Structure
        //exportKeyword >>. ws1 >>? Structures.functionDefnition |>> ExportStatement.Structure
    ]
    |>> Statement.Export