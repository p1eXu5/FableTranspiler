module FableTranspiler.Parsers.Import


open FParsec
open Types
open Common


let importKeyWord = str "import"

let asterisk = pchar '*'


let openBrace : Parser<char, unit> = pchar '{'
let closedBrace : Parser<char, unit> = pchar '}'


let namedEntity = Common.identifier .>>? notFollowedByL (str " as") "named entity must not followed by 'as'" |>> ImportEntity.Named


let aliased = 
    Common.identifier 
        .>> ws1
        .>>? str "as " 
        .>>.? identifier 
        |>> (fun t -> ((fst t), snd t) |> ImportEntity.Aliased)



let all = asterisk .>> ws1 |>> (fun _ -> ImportEntity.All)

let allAliased = 
    asterisk 
        >>? ws1 
        >>? str "as"
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




let importStatement =
    ws 
        >>. importKeyWord 
        >>. choice [
            ws1  
                >>? choice [
                    openBrace >>? ws >>. many entity .>> ws .>> closedBrace
                    allAliased |>> List.singleton
                ]
                .>> ws1 
                .>> str "from "

            noEntity |>> List.singleton
        ]
        .>>. choice [
            relativeModule
            nodeModule
        ]
        .>> pchar ';'
        |>> Statement.Import