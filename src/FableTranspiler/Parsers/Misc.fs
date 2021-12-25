module FableTranspiler.Parsers.Misc

open FParsec
open Types
open Common


let declareConst =
    ws
    >>? skipString "declare const"
    >>. ws1
    >>. identifier
    .>> ws
    .>> skipChar ':'
    .>> ws
    .>>. Structures.typeDefinition 
    |>> Statement.DeclareConst
    .>> skipChar ';'



