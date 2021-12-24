module FableTranspiler.Parsers.Literals

open FParsec
open Types
open Common


let emptyObjectLiteral<'a> : Parser<unit, 'a>= skipChar '{' .>> ws .>> skipChar '}'


