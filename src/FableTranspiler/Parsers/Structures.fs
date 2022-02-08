/// class, interface and type composition
[<RequireQualifiedAccess>]
module FableTranspiler.Parsers.Structures

open FParsec
open Types
open Common


let field, fieldR = createParserForwardedToRef()
let ``type``, typeR = createParserForwardedToRef()
let typeDefinition, typeDefinitionR = createParserForwardedToRef()

let notFollowedByChars chars = notFollowedBy (skipAnyOf chars)
let attemptSep ch = attempt (ws >>. skipChar ch >>. ws)


let typeParams = 
    skipChar '<'
    >>. ws
    >>. sepBy1 identifier (attemptSep ',')
    .>> ws
    .>> skipChar '>'


let funcParams =
    (sepEndBy field (attempt(ws .>> skipChar ',' .>> ws)) |>> (fun t -> t : FieldList))


let funcSignature =
    skipChar '('
    >>. ws
    >>. funcParams
    .>> ws
    .>> skipChar ')'
    .>> ws
    .>> skipChar ':'
    .>> ws
    .>>. typeDefinition


let emptyObjectLiteral<'a> : Parser<unit, 'a>= skipChar '{' .>> ws .>>? skipChar '}'


let objectLiteral : Parser<FieldList, _> = 
    skipChar '{' 
    >>. ws
    >>. sepEndBy1 (field) (attempt(ws >>. skipChar ';' >>. ws) <|> (ws1 : Parser<unit, unit>))
    .>> (skipChar '}' <?> "object literal must be terminated by '}'")


let qualifiers = sepBy1 identifier (attemptSep '.') 

let planeType = qualifiers .>>? notFollowedByChars ['<'] |>> DTsType.Plain

let genericType = 
    qualifiers
    .>> ws
    .>>? skipChar '<' .>> ws .>>. sepBy ``type`` (attemptSep ',') .>> ws .>> skipChar '>'
    |>> DTsType.Generic


let funcType =
    skipChar '('
    >>. ws
    >>. funcParams
    .>> ws
    .>> skipChar ')'
    .>> ws
    .>> skipString "=>"
    .>> ws
    .>>. typeDefinition
    
    |>> DTsType.Func


let typeof =
    skipString "typeof"
    >>. ws1
    >>. qualifiers
    |>> DTsType.Typeof



do 
    typeR.Value <-
        choiceL [
            skipString "void" |>> (fun _ -> DTsType.Void)
            skipString "undefined" |>> (fun _ -> DTsType.Undefined)
            (skipString "any" |>> (fun _ -> DTsType.Any)) .>>? skipString "[]" |>> DTsType.Array
            skipString "any" |>> (fun _ -> DTsType.Any)
            typeof
            planeType .>>? skipString "[]" |>> DTsType.Array
            planeType
            genericType
            skipChar '(' >>. ws >>? funcType .>> ws .>> skipChar ')'
            funcType
            emptyObjectLiteral |>> (fun _ -> InlineObject [])
            objectLiteral |>> InlineObject
        ] "expecting type name"


let typeKeyword = skipString "type"


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

do 
    typeDefinitionR.Value <-
        choice [
            typeCombination |>> Combination
            ``type`` |>> Single
        ]







[<AutoOpen>]
module Fields = 

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
        .>>.? funcSignature 
        |>> (fun t ->
            let (i, (f, td)) = t
            (i, f, td)
        )

    let funcOpt =
        identifier
        .>>? skipChar '?'
        .>> ws
        .>>.? funcSignature
        |>> (fun t ->
            let (i, (f, td)) = t
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


[<AutoOpen>]
module TypeAlias =

    let plainTypeAlias =
        typeKeyword 
        >>. ws1 
        >>. identifier 
        .>> ws 
        .>>? (skipChar '=' <?> "expecting '=' in plain type alias")
        .>> ws 
        .>>. typeCombination
        .>> skipChar ';'
        |>> (TypeAlias.Plain >> StructureStatement.TypeAlias)


    let genericTypeAlias =
        typeKeyword 
        >>. ws1 
        >>. identifier 
        .>> ws
        .>>.? typeParams
        .>> ws
        .>> (skipChar '=' <?> "expecting '=' in generic type alias")
        .>> ws 
        .>>. typeCombination
        .>> skipChar ';'
        |>> (fun t ->
            let ((i, xi), tc) = t
            TypeAlias.Generic (i, xi, tc) |> StructureStatement.TypeAlias
        )

    let typeAlias =
        choice [
            genericTypeAlias
            plainTypeAlias
        ]


[<AutoOpen>]
module InterfaceClassDefinition =

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


[<AutoOpen>]
module FunctionDefinition =

    let plainFunctionDefnition =
        skipString "function"
        >>? ws1
        >>? funcReq
        |>> (FunctionDefinition.Plain >> StructureStatement.FunctionDefinition)
        .>> skipChar ';'

    let genericFunctionDefnition =
        skipString "function"
        >>? ws1
        >>? identifier
        .>> ws
        .>>.? typeParams
        .>>.? funcSignature 
        .>> skipChar ';'
        |>> (fun t ->
            let ((i, il), (fl, td)) = t
            (i, il, fl, td)
            |> FunctionDefinition.Generic 
            |> StructureStatement.FunctionDefinition
        )

    let genericNamelesssFunctionDefnition =
        skipString "function"
        .>> ws
        >>? typeParams
        .>>.? funcSignature 
        .>> skipChar ';'
        |>> (fun t ->
            let (il, (fl, td)) = t
            (il, fl, td)
            |> FunctionDefinition.GenericNameless 
            |> StructureStatement.FunctionDefinition
        )

    let functionDefnition =
        choice [
            genericFunctionDefnition
            plainFunctionDefnition
            genericNamelesssFunctionDefnition
        ]


[<AutoOpen>]
module ConstDefinition =

    let declareConst =
        skipString "declare const"
        >>. ws1
        >>. identifier
        .>> ws
        .>> skipChar ':'
        .>> ws
        .>>. typeDefinition 
        |>> ConstDefinition.DeclareConst
        .>> skipChar ';'



    let ``const`` =
        skipString "const"
        >>? ws1
        >>. identifier
        .>> ws
        .>>? skipChar ':'
        .>> ws
        .>>. typeDefinition
        |>> ConstDefinition.Const
        .>> skipChar ';'


    let constDefinition =
        choice [
            declareConst
            ``const``
        ] |>> StructureStatement.ConstDefinition


let statement =
    choice [
        typeAlias
        classDefinition
        interfaceDefinition
        functionDefnition
        constDefinition
    ]