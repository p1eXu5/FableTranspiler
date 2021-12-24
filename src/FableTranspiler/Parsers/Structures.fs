/// class, interface and type composition
[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Structures

open FParsec
open Types
open Common

let notFollowedByChars chars = notFollowedBy (skipAnyOf chars)
let attemptSep ch = attempt (ws >>. skipChar ch >>. ws)
let qualifiers = sepBy identifier (attemptSep '.') 




let planeType = qualifiers .>>? notFollowedByChars ['<'] |>> TypeName.Plain

let genericType = 
    qualifiers
    .>> ws
    .>>? skipChar '<' .>> ws .>>. sepBy planeType (attemptSep ',') .>> ws .>> skipChar '>'
    |>> TypeName.Generic

let ``type`` =
    choiceL [
        skipString "undefined" |>> (fun _ -> TypeName.Undefined)
        planeType
        genericType
    ] "expecting type name"


let typeKeyword = skipString "type"

// operators could be added

let typeComposition =
    ``type``
    .>> ws
    .>>? skipChar '&'
    .>> ws
    .>>. sepBy1 ``type`` (attemptSep '&') 
    |>> (fun tpl -> 
        let (t, l) = tpl
        (t :: l) |> TypeCombination.Composition
    )

let typeUnion = 
    ``type``
    .>> ws
    .>>? skipChar '|'
    .>> ws
    .>>. sepBy1 ``type`` (attemptSep '|') 
    |>> (fun tpl -> 
        let (t, l) = tpl
        (t :: l) |> TypeCombination.Union
    )

let typeCombination =
    choice [
        typeComposition
        typeUnion // order make sense
    ]


let typeAlias =
    typeKeyword >>. ws1 >>. identifier .>> ws .>> skipChar '=' .>> ws 
        .>>. typeCombination
        .>> skipChar ';'
        |>> TypeAlias


let typeReference =
    choice [
        typeCombination |>> Combination
        ``type`` |>> Single
    ]
    .>> skipChar ';'


let typeDefinitionStructure =
    identifier 
    .>> ws
    .>>? skipChar ':' 
    .>> ws
    .>>. typeReference


let optionTypeDefinitionStructure =
    identifier
    .>>? skipChar '?'
    .>> ws
    .>> skipChar ':' 
    .>> ws
    .>>. typeReference


let emptyObjectLiteral<'a> : Parser<unit, 'a>= skipChar '{' .>> ws .>> skipChar '}'

let fieldReq = 
    (identifier |>> Required) 
    .>> ws 
    .>>? skipChar ':'
    .>> ws
    .>>. typeReference

let fieldOpt = 
    (identifier |>> Optional)
    .>>? skipChar '?'
    .>> ws 
    .>> skipChar ':'
    .>> ws
    .>>. typeReference

let field = 
    choice [
        fieldReq
        fieldOpt
    ]

    
let objectLiteral = 
    skipChar '{' 
    >>. ws
    >>. sepEndBy1 field (ws >>. skipChar ';' >>. newline)
    .>> ws
    .>> skipChar '}'



let classKeyword = skipString "class"
let extendsKeyword = skipString "extends"

let classDefinition = 
    classKeyword
    >>. ws1
    >>. identifier
    .>> ws1
    .>> extendsKeyword
    .>> ws1
    .>>. ``type``
    .>> ws
    .>> emptyObjectLiteral
    |>> ClassDefinition.ExtendsEmpty
    |>> ClassDefinition

let statement =
    choice [
        skipString "export" >>? ws1 >>? typeAlias // TODO: move to exports
        typeAlias
        classDefinition
    ]
    |>> Structure