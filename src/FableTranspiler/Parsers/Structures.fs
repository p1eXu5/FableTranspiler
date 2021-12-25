/// class, interface and type composition
[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Structures

open FParsec
open Types
open Common

let notFollowedByChars chars = notFollowedBy (skipAnyOf chars)
let attemptSep ch = attempt (ws >>. skipChar ch >>. ws)
let qualifiers = sepBy1 identifier (attemptSep '.') 


let field, fieldR = createParserForwardedToRef()
let typeDefinition, typeDefinitionR = createParserForwardedToRef()


let funcParams =
    (sepBy field (attempt(ws .>> skipChar ',' .>> ws)) |>> (fun t -> t : FieldList))


let planeType = qualifiers .>>? notFollowedByChars ['<'] |>> DTsType.Plain

let genericType = 
    qualifiers
    .>> ws
    .>>? skipChar '<' .>> ws .>>. sepBy planeType (attemptSep ',') .>> ws .>> skipChar '>'
    |>> DTsType.Generic


let funcType =
    skipChar '('
    >>. ws
    >>? skipChar '('
    >>. ws
    >>. funcParams
    .>> ws
    .>> skipChar ')'
    .>> ws
    .>> skipString "=>"
    .>> ws
    .>>. typeDefinition
    .>> ws
    .>> skipChar ')'
    |>> DTsType.Func


let typeof =
    skipString "typeof"
    >>. ws1
    >>. identifier
    |>> DTsType.Typeof



let ``type`` =
    choiceL [
        skipString "void" |>> (fun _ -> DTsType.Void)
        skipString "undefined" |>> (fun _ -> DTsType.Undefined)
        skipString "any" |>> (fun _ -> DTsType.Any)
        typeof
        planeType
        genericType
        funcType
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


do 
    typeDefinitionR.Value <-
        choice [
            typeCombination |>> Combination
            ``type`` |>> Single
        ]

let emptyObjectLiteral<'a> : Parser<unit, 'a>= skipChar '{' .>> ws .>> skipChar '}'


let fieldReq = 
    (identifier |>> Required) 
    .>> ws 
    .>>? skipChar ':'
    .>> ws
    .>>. typeDefinition


let fieldOpt = 
    (identifier |>> Optional)
    .>>? skipChar '?'
    .>> ws 
    .>>? skipChar ':'
    .>> ws
    .>>. typeDefinition



let funcReq =
    identifier
    .>> ws
    .>>? skipChar '('
    .>> ws
    .>>. funcParams
    .>> ws
    .>> skipChar ')'
    .>> ws
    .>> skipChar ':'
    .>> ws
    .>>. typeDefinition
    |>> (fun t ->
        let ((i, f), td) = t
        (i, f, td)
    )

let funcOpt =
    identifier
    .>>? skipChar '?'
    .>> ws
    .>>? skipChar '('
    .>> ws
    .>>. funcParams
    .>> ws
    .>> skipChar ')'
    .>> ws
    .>> skipChar ':'
    .>> ws
    .>>. typeDefinition
    |>> (fun t ->
        let ((i, f), td) = t
        (i, f, td)
    )



do 
    fieldR.Value <- 
        choice [
            fieldReq
            fieldOpt
            funcOpt |>> (fun t -> let (i, f, td) = t in FuncOpt (i, f), td)
            funcReq |>> (fun t -> let (i, f, td) = t in FuncReq (i, f), td)
        ]


let objectLiteralField =
    field
    .>> (skipChar ';' <?> "field must be terminated by ';'")

    
let objectLiteral : Parser<FieldList, _> = 
    skipChar '{' 
    >>. ws
    >>. sepEndBy1 (objectLiteralField) (newline .>> ws)
    .>> ws
    .>> (skipChar '}' <?> "object literal must be terminated by '}'")



let interfaceKeyword = skipString "interface"
let classKeyword = skipString "class"
let extendsKeyword = skipString "extends"

let extendsEmptyClassDefinition = 
    (ClassDefinition.ExtendsEmpty >> StructureStatement.ClassDefinition)


let extendsInterfaceDefinition = 
    (InterfaceDefinition.Extends >> StructureStatement.InterfaceDefinition)

let plainInterfaceDefinition = 
    (InterfaceDefinition.Plain >> StructureStatement.InterfaceDefinition)


let extendsEmpty keyword map = 
    keyword
    >>. ws1
    >>. identifier
    .>> ws1
    .>>? extendsKeyword
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
    .>>? extendsKeyword
    .>> ws1
    .>>. ``type``
    .>> ws
    .>>. objectLiteral
    |>> (fun t -> 
        let ((i, tn), ol) = t
        map (i, tn, ol)
    )


let plain keyword map = 
    keyword
    >>. ws1
    >>. identifier
    .>> ws1
    .>>. objectLiteral
    |>> map


let classDefinition =
    extendsEmpty classKeyword extendsEmptyClassDefinition

let interfaceDefinition =
    choice [
        extends interfaceKeyword extendsInterfaceDefinition
        plain interfaceKeyword plainInterfaceDefinition
    ]

let functionDefnition =
    skipString "function"
    >>. ws
    >>. funcReq
    |>> StructureStatement.FunctionDefinition
    .>> skipChar ';'


let statement =
    choice [
        typeAlias
        classDefinition
        interfaceDefinition
        functionDefnition
    ]