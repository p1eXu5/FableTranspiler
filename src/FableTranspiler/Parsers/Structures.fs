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


//let typeDefinitionStructure =
//    identifier 
//    .>> ws
//    .>>? skipChar ':' 
//    .>> ws
//    .>>. typeReference


//let optionTypeDefinitionStructure =
//    identifier
//    .>>? skipChar '?'
//    .>> ws
//    .>> skipChar ':' 
//    .>> ws
//    .>>. typeReference


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
    .>> (skipChar ';' <?> "field must be terminated by ';'")

    
let objectLiteral : Parser<ObjectLiteral, _> = 
    skipChar '{' 
    >>. ws
    >>. sepEndBy1 (field) (newline .>> ws)
    .>> ws
    .>> (skipChar '}' <?> "object literal must be terminated by '}'")



let interfaceKeyword = skipString "interface"
let classKeyword = skipString "class"
let extendsKeyword = skipString "extends"

let extendsEmptyClassDefinition = 
    (ClassDefinition.ExtendsEmpty >> StructureStatement.ClassDefinition)


let extendsInterfaceDefinition = 
    (InterfaceDefinition.Extends >> StructureStatement.InterfaceDefinition)



let extendsEmpty keyword map = 
    keyword
    >>. ws1
    >>. identifier
    .>> ws1
    .>> extendsKeyword
    .>> ws1
    .>>. ``type``
    .>> ws
    .>> emptyObjectLiteral
    |>> map


let extends keyword map = 
    keyword
    >>. ws1
    >>. identifier
    .>> ws1
    .>> extendsKeyword
    .>> ws1
    .>>. ``type``
    .>> ws
    .>>. objectLiteral
    |>> (fun t -> 
        let ((i, tn), ol) = t
        map (i, tn, ol)
    )


let classDefinition =
    extendsEmpty classKeyword extendsEmptyClassDefinition

let interfaceDefinition =
    extends interfaceKeyword extendsInterfaceDefinition


let statement =
    choice [
        skipString "export" >>? ws1 >>? typeAlias // TODO: move to exports
        typeAlias
        classDefinition
        interfaceDefinition
    ]
    |>> Structure