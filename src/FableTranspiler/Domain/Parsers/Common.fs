module FableTranspiler.Parsers.Common

open FParsec
open Types
open FableTranspiler.SimpleTypes

///<summary>
/// Skips over any sequence of *zero* or more whitespaces (space (' '), tab ('\t')
/// or newline ("\n", "\r\n" or "\r")).
///</summary> 
let ws    = spaces
let ws1   = spaces1
let str s = pstring s


let notValidIdentifierSymbols = ['-'; '/'; '\\'; '#'; '{'; '}'; '('; ')'; '?'; '+'; '*'; ':'; ';'; '~'; '!'; '&'; '|']

let quote : Parser<_, unit> = skipChar '\''
let doubleQuote : Parser<_, unit> = skipChar '\"'

let skipQuote = quote <|> doubleQuote

let openBrace : Parser<unit, unit> = skipChar '{'
let closedBrace : Parser<unit, unit> = skipChar '}'

let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'

let identifier : Parser<Identifier, unit> =

    FParsec.CharParsers.identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))
        |>> Identifier.create

let attemptSep ch = attempt (ws >>. skipChar ch >>. ws)

let qualifiers = sepBy1 identifier (attemptSep '.')