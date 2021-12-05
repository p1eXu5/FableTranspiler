module FableTranspiler.Parsers.Import


open FParsec
open Types
open Common


let importKeyWord = str "import"

let asterisk = pchar '*'


let openBrace : Parser<char, unit> = pchar '{'
let closedBrace : Parser<char, unit> = pchar '}'

let identifier = 
    many1CharsTill anyChar (ws1 <|> followedBy (anyOf notValidIdentifierSymbols))
        |>> Identifier.Create

let namedEntity = identifier |>> ImportEntity.Named

let aliasEntity = 
    many1CharsTill anyChar ws1 
        .>> str "as" 
        .>>. identifier 
        |>> (fun t -> ((Identifier.Create (fst t)), snd t) |> ImportEntity.Aliased)



let defaultEntity = asterisk .>> ws1 |>> (fun _ -> ImportEntity.All)

let defaultAliasedEntity = 
    asterisk 
        >>. ws1 
        >>. str "as"
        >>. ws1
        >>. identifier 
        |>> ImportEntity.AllAliased

let noEntity = ws1 |>> (fun _ -> ImportEntity.No)


let entity =
    choice [
        namedEntity
        aliasEntity
        defaultEntity
        defaultAliasedEntity
        noEntity
    ]

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
    ws >>. importKeyWord >>. ws1  
        >>. choice [
            openBrace >>? ws >>. many entity .>> ws .>> closedBrace
            defaultAliasedEntity |>> List.singleton
        ]
        .>> choice [
            ws1
            str "from" >>. ws1
        ]
        .>>. choice [
            relativeModule
            nodeModule
        ]
        .>> pchar ';'
        |>> Statement.Import