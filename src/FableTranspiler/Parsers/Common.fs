module FableTranspiler.Parsers.Common

open FParsec
open Types

let ws    = spaces
let ws1   = spaces1
let str s = pstring s


let notValidIdentifierSymbols = ['-'; '/'; '\\'; '#'; '{'; '}'; '('; ')'; '?'; '+'; '*'; ':'; ';'; '~'; '!'; '&'; '|']

let quote : Parser<_, unit> = skipChar '\''
let doubleQuote : Parser<_, unit> = skipChar '\"'

let openBrace : Parser<unit, unit> = skipChar '{'
let closedBrace : Parser<unit, unit> = skipChar '}'


let identifier : Parser<Identifier, unit> = 
    FParsec.CharParsers.identifier (IdentifierOptions())
        |>> Identifier.Create