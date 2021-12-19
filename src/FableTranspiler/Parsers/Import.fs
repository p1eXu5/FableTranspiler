[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Import

open FParsec
open Types
open Common


let keyword = skipString "import"

let asterisk = skipChar '*'


let namedEntity = 
    Common.identifier .>>? notFollowedByL (str " as") "named entity must not followed by 'as'" |>> ImportEntity.Named


let aliased = 
    Common.identifier 
        .>> ws1
        .>>? skipString "as " 
        .>>.? identifier 
        |>> (fun t -> ((fst t), snd t) |> ImportEntity.Aliased)



let all = asterisk .>> ws1 |>> (fun _ -> ImportEntity.All)

let allAliased = 
    asterisk 
        >>? ws1 
        >>? skipString "as"
        >>? ws1
        >>? identifier 
        |>> ImportEntity.AllAliased

let noEntity : Parser<ImportEntity, unit> = ws1 |>> (fun _ -> ImportEntity.No)


let entity =
    ws
    >>. choice [
        namedEntity
        aliased
        all
        allAliased
    ]
    .>> ws




let statement =
    ws 
        >>. keyword 
        >>. choice [
            ws1  
                >>? choice [
                    openBrace >>? ws >>. sepEndBy1 entity (skipChar ',') .>> ws .>> closedBrace
                    allAliased |>> List.singleton
                ]
                .>> ws1 
                .>> skipString "from "

            noEntity |>> List.singleton
        ]
        .>>. choice [
            Module.relative
            Module.node
        ]
        .>> ws
        .>> skipChar ';'
        |>> Statement.Import