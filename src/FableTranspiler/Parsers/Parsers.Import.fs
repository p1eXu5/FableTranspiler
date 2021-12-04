module FableTranspiler.Parsers.Import


open FParsec
open Types
open Common


let importKeyWord = str "import"

let asterisk = pchar '*'


let openBrace = pchar '{'
let closedBrace = pchar '}'

let identifier = 
    many1CharsTill anyChar (ws1 <|> skipSatisfy (isAnyOf notValidIdentifierSymbols))
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

let relativeModule = quote >>. (pchar '.' <|> pchar '~') >>. many1CharsTill anyChar quote |>> (fun path -> System.Uri(path, UriKind.Relative) |> Relative)
let nodeModule = 
    quote 
    >>. noneOf ['.'; '~'] 
    .>>. many1CharsTill anyChar quote 
    |>> (fun path -> System.Uri((sprintf "%c%s" (fst path) (snd path)).Replace("~", "."), UriKind.Relative) |> NodeModule)


let importStatement =
    ws >>. importKeyWord >>. ws1  
        >>. choice [
            openBrace >>. ws >>. many entity .>> ws .>> closedBrace
            entity |>> List.singleton
        ]
        .>> choice [
            ws1
            ws1 .>> str "from" .>> ws1
        ]
        .>>. choice [
            relativeModule
            nodeModule
        ]
        .>> pchar ';'
        |>> Statement.Import