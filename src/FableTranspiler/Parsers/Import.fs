﻿[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Import

open FParsec
open Types
open Common


let importKeyWord = skipString "import"

let asterisk = skipChar '*'


let openBrace : Parser<unit, unit> = skipChar '{'
let closedBrace : Parser<unit, unit> = skipChar '}'


let namedEntity = Common.identifier .>>? notFollowedByL (str " as") "named entity must not followed by 'as'" |>> ImportEntity.Named


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

open System

let relativeModule = 
    quote 
    >>? (pchar '.') 
    .>>.? many1CharsTill anyChar quote 
    |>> (fun path -> ModulePath ((sprintf "%c%s" (fst path) (snd path))) |> Relative)

let nodeModule = 
    quote 
    >>? (noneOf ['.'; '/'; '\\'] <|> satisfy (fun ch -> ch = '~' || Char.IsLetterOrDigit(ch)))
    .>>.? many1CharsTill anyChar quote 
    |>> (fun path -> ModulePath ((sprintf "%c%s" (fst path) (snd path))) |> NodeModule)




let statement =
    ws 
        >>. importKeyWord 
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
            relativeModule
            nodeModule
        ]
        .>> ws
        .>> skipChar ';'
        |>> Statement.Import