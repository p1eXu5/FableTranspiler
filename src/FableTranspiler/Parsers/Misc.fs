module FableTranspiler.Parsers.Misc

open FParsec
open Types
open Common


let declareConst =
    skipString "declare const"
    >>. ws1
    >>. identifier
    .>> ws
    .>> skipChar ':'
    .>> ws
    .>>. Structures.typeDefinition 
    |>> Statement.DeclareConst
    .>> skipChar ';'



let constDefinition =
    skipString "const"
    >>? ws1
    >>. identifier
    .>> ws
    .>>? skipChar ':'
    .>> ws
    .>>. Structures.typeDefinition
    |>> Statement.ConstDefinition
    .>> skipChar ';'
