module FableTranspiler.Parsers.Common

open FParsec
open Types

let ws    = spaces
let ws1   = spaces1
let str s = pstring s


let notValidIdentifierSymbols = ['-'; '/'; '\\'; '#'; '{'; '}'; '('; ')'; '?'; '+'; '*'; ':'; ';'; '~'; '!']

let quote : Parser<_, unit> = skipChar '\''
