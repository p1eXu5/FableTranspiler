﻿module internal rec FableTranspiler.VmAdapters.DtsDocumentInterpreter

open FableTranspiler.Parsers.Types
open System.Linq


type EntityOrder =
    | Single
    | First
    | Last
    | Middle

let private modulePath = function
    | NodeModule (ModulePath path) -> { Tag = Tag.Text; Content = sprintf "'%s'" path}
    | Relative (ModulePath path) -> { Tag = Tag.Text; Content = sprintf "'%s'" path}


let private insertWithOrder entityOrder posfix content =
    match entityOrder with
    | Single -> sprintf "{ %s }%s" content posfix
    | First -> sprintf "{ %s, " content
    | Last -> sprintf "%s }%s" content posfix
    | Middle -> sprintf "%s, " content


let private insertAsWithOrder entityOrder nameTag name alias postfix =
    match entityOrder with
    | First ->
        [
            { Tag = Tag.Text; Content = "{ "}
            { Tag = nameTag; Content = name }
            { Tag = Tag.Keyword; Content = " as "}
            { Tag = Tag.Type; Content = alias}
            { Tag = Tag.Text; Content = ", "}
        ]
    | Last ->
        [
            { Tag = nameTag; Content = name }
            { Tag = Tag.Keyword; Content = " as "}
            { Tag = Tag.Type; Content = alias}
            { Tag = Tag.Text; Content = " }" + postfix}
        ]
    | Middle ->
        [
            { Tag = nameTag; Content = name }
            { Tag = Tag.Keyword; Content = " as "}
            { Tag = Tag.Type; Content = alias}
            { Tag = Tag.Text; Content = ", "}
        ]
    | Single ->
        [
            { Tag = Tag.Text; Content = "{ "}
            { Tag = nameTag; Content = name }
            { Tag = Tag.Keyword; Content = " as "}
            { Tag = Tag.Type; Content = alias}
            { Tag = Tag.Text; Content = " }" + postfix}
        ]


let private importEntity postfix entity entityOrder = 
    match entity with
    | No -> [{ Tag = Tag.NoContent; Content = null }]
    | ImportEntity.Named (Identifier identifier) -> [{ Tag = Tag.Text; Content = identifier |> insertWithOrder entityOrder postfix}]
    | Aliased (Identifier name, Identifier alias) -> insertAsWithOrder entityOrder Tag.Text name alias postfix
    | All -> [{ Tag = Tag.Text; Content = "* "}]
    | AllAliased (Identifier alias) -> 
        [
            { Tag = Tag.Text; Content = "* "}
            { Tag = Tag.Keyword; Content = "as "}
            { Tag = Tag.Type; Content = sprintf "%s " alias}
        ]


let private exportEntity postfix entity entityOrder = 
    match entity with
    | ExportEntity.Named (Identifier identifier) -> [{ Tag = Tag.Text; Content = identifier |> insertWithOrder entityOrder postfix}]
    | DefaultAliased (Identifier alias) -> insertAsWithOrder entityOrder Tag.Modifier "default" alias postfix


let rec private buildImportExportEntities builder l res =
    match l with
    | head::[] when res |> List.isEmpty -> 
        builder head Single

    | head::[] when res |> List.isEmpty |> not -> 
        res @ builder head Last

    | head::tail when res |> List.isEmpty ->
        let res' = res @ builder head First
        buildImportExportEntities builder tail res'

    | head::tail when res |> List.isEmpty |> not ->
        let res' = res @ builder head Middle
        buildImportExportEntities builder tail res'

    | _ -> []
                

let private constructType l : DocumentSegmentViewModel list =
    l
    |> List.map (fun t -> [{ Tag = Tag.Type; Content = Identifier.Value(t)}])
    |> List.reduce (fun t1 t2 -> 
        [
            yield! t1
            { Tag = Tag.Text; Content = "." }
            yield! t2
        ]
    )


let private constructSingleType tn =
    constructCombination "" [tn] []


let rec private constructObjectLiteral (fields: (Field * TypeDefinition) list) (endLine: bool * string) =
    fields
    |> List.mapi (fun ind l ->
        let (fld, tdef) = l
        [
            if fst endLine then
                yield { Tag = Tag.Text; Content = "    " }

            match fld with
            | Required (Identifier i) ->
                yield { Tag = Tag.Text; Content = i }
                yield { Tag = Tag.Parentheses; Content = ": " }
            | Optional (Identifier i) ->
                yield { Tag = Tag.Text; Content = i }
                yield { Tag = Tag.Parentheses; Content = "?: " }
            | FuncOpt (Identifier i, fl) ->
                yield { Tag = Tag.Text; Content = i }
                yield { Tag = Tag.Parentheses; Content = "?(" }
                yield! constructObjectLiteral fl (false, ", ")
                yield { Tag = Tag.Parentheses; Content = "): " }
            | FuncReq (Identifier i, fl) ->
                yield { Tag = Tag.Text; Content = i }
                yield { Tag = Tag.Parentheses; Content = "(" }
                yield! constructObjectLiteral fl (false, ", ")
                yield { Tag = Tag.Parentheses; Content = "): " }

            yield! constructTypeDefinition tdef
            //match tdef with
            //| TypeDefinition.Single tn -> 
            //    yield! constructSingleType tn
            //| TypeDefinition.Combination comb ->
            //    match comb with
            //    | Union l ->
            //        yield! constructCombination " | " l []
            //    | Composition l ->
            //        yield!  constructCombination " & " l [] 



            if fst endLine then
                yield { Tag = Tag.EndOfLine; Content = snd endLine }
            elif ind < (fields.Length - 1) then
                yield { Tag = Tag.Text; Content = snd endLine }
        ]
    )
    |> List.concat


let constructTypeDefinition tdef =
    [
        match tdef with
        | TypeDefinition.Single tn -> 
            yield! constructSingleType tn
        | TypeDefinition.Combination comb ->
            match comb with
            | Union l ->
                yield! constructCombination " | " l []
            | Composition l ->
                yield!  constructCombination " & " l [] 
    ]


let rec private constructCombination sep combination res =
    match combination with
    | [] -> 
        res
        |> List.rev
        |> List.reduce (fun t1 t2 -> 
            [
                yield! t1
                { Tag = Tag.Parentheses; Content = sep }
                yield! t2
            ]
        )
    | head :: tail ->
        match head with
        | DTsType.InlineObject fl ->
            [
                { Tag = Tag.Parentheses; Content = "{" }
                yield! constructObjectLiteral fl (false, ", ")
                { Tag = Tag.Parentheses; Content = "}" }
            ]
                

        | DTsType.Plain p -> 
            constructType p
            |> (fun r -> constructCombination sep tail (r :: res))
                    
        | DTsType.Generic (p, i) ->
            let main = constructType p
            let inner = constructCombination ", " i []
            let l =
                [
                    yield! main
                    { Tag = Tag.Text; Content = "<" }
                    yield! inner
                    { Tag = Tag.Text; Content = ">" }
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Undefined -> 
            let l =
                [
                    { Tag = Tag.Type; Content = "undefined" }
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Void -> 
            let l =
                [
                    { Tag = Tag.Type; Content = "void" }
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Any -> 
            let l =
                [
                    { Tag = Tag.Type; Content = "any" }
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Func (fl, tdef) ->
            let l =
                [
                    { Tag = Tag.Parentheses; Content = "((" }
                    yield! constructObjectLiteral fl (false, ", ")
                    { Tag = Tag.Parentheses; Content = ") => " }

                    yield! constructTypeDefinition tdef
                    { Tag = Tag.Parentheses; Content = ")" }
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Typeof (Identifier i) ->
            let l =
                [
                    { Tag = Tag.Keyword; Content = "typeof " }
                    { Tag = Tag.Text; Content = i }
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Array t ->
            let l = 
                [
                    yield! constructSingleType t
                    {Tag = Tag.Parentheses; Content = "[]"}
                ]
            constructCombination sep tail (l :: res)



let constructTypeParams typeParams =
    typeParams
    |> List.mapi (fun ind (Identifier i) ->
        [
            yield { Tag = Tag.Type; Content = i }
            if ind < (typeParams.Length - 1) then
                yield { Tag = Tag.Text; Content = ", " }
        ]
    )
    |> List.concat


let rec private interpretStructure structure : DocumentSegmentViewModel list =
    match structure with
    | TypeAlias (TypeAlias.Plain (Identifier identifier, combination)) ->
        [
            yield { Tag = Tag.Keyword; Content = "type " }
            yield { Tag = Tag.Type; Content = identifier }
            yield { Tag = Tag.Text; Content = " = " }
            match combination with
            | Union l ->
                yield! constructCombination " | " l []
            | Composition l ->
                yield!  constructCombination " & " l []
            yield { Tag = Tag.EndOfLine; Content = ";" }
        ]

    | TypeAlias (TypeAlias.Generic (Identifier identifier, typeParams, combination)) ->
        [
            yield { Tag = Tag.Keyword; Content = "type " }
            yield { Tag = Tag.Type; Content = identifier }
            yield { Tag = Tag.Parentheses; Content = "<" }
            yield! constructTypeParams typeParams 
            yield { Tag = Tag.Text; Content = " = " }
            match combination with
            | Union l ->
                yield! constructCombination " | " l []
            | Composition l ->
                yield!  constructCombination " & " l []
            yield { Tag = Tag.EndOfLine; Content = ";" }
        ]

    | ClassDefinition (ExtendsEmpty (Identifier idetifier, tn)) -> 
        [
            yield { Tag = Tag.Keyword; Content = "class " }
            yield { Tag = Tag.Type; Content = idetifier }
            yield { Tag = Tag.Keyword; Content = " extends " }

            yield! constructSingleType tn // construct combination from single operand

            yield { Tag = Tag.Parentheses; Content = "{}" }
            yield { Tag = Tag.EndOfLine; Content = null }
        ]

    | InterfaceDefinition (Extends (Identifier idetifier, tn, lit)) ->
        [
            yield { Tag = Tag.Keyword; Content = "interface " }
            yield { Tag = Tag.Type; Content = idetifier }
            yield { Tag = Tag.Keyword; Content = " extends " }
                    
            yield! constructSingleType tn  // construct combination from single operand

            yield { Tag = Tag.Parentheses; Content = "{" }
            yield { Tag = Tag.EndOfLine; Content = null }
                    
            yield! constructObjectLiteral lit (true, ", ")
                    
            yield { Tag = Tag.Parentheses; Content = "}" }
            yield { Tag = Tag.EndOfLine; Content = null }
        ]

    | InterfaceDefinition (InterfaceDefinition.Plain (Identifier idetifier, fl)) ->
        [
            yield { Tag = Tag.Keyword; Content = "interface " }
            yield { Tag = Tag.Type; Content = idetifier }

            yield { Tag = Tag.Parentheses; Content = "{" }
            yield { Tag = Tag.EndOfLine; Content = null }
                    
            yield! constructObjectLiteral fl (true, ", ")
                    
            yield { Tag = Tag.Parentheses; Content = "}" }
            yield { Tag = Tag.EndOfLine; Content = null }
        ]

    | FunctionDefinition (FunctionDefinition.Plain ((Identifier i), fl, tdef)) ->
        [
            yield { Tag = Tag.Keyword; Content = "function " }
            yield { Tag = Tag.Type; Content = i }

            yield { Tag = Tag.Parentheses; Content = "(" }
                    
            yield! constructObjectLiteral fl (false, ", ")
                    
            yield { Tag = Tag.Parentheses; Content = ")" }
            yield { Tag = Tag.Text; Content = ": " }

            yield! constructTypeDefinition tdef
            yield { Tag = Tag.EndOfLine; Content = ";" }
        ]

    | FunctionDefinition (FunctionDefinition.Generic ((Identifier i), il, fl, tdef)) ->
        [
            yield { Tag = Tag.Keyword; Content = "function " }
            yield { Tag = Tag.Type; Content = i }
            yield { Tag = Tag.Parentheses; Content = "<" }
            yield! constructTypeParams il
            yield { Tag = Tag.Parentheses; Content = ">" }

            yield { Tag = Tag.Parentheses; Content = "(" }
                    
            yield! constructObjectLiteral fl (false, ", ")
                    
            yield { Tag = Tag.Parentheses; Content = ")" }
            yield { Tag = Tag.Text; Content = ": " }

            yield! constructTypeDefinition tdef
            yield { Tag = Tag.EndOfLine; Content = ";" }
        ]

    | FunctionDefinition (FunctionDefinition.GenericNameless (il, fl, tdef)) ->
        [
            yield { Tag = Tag.Keyword; Content = "function" }
            yield { Tag = Tag.Parentheses; Content = "<" }
            yield! constructTypeParams il
            yield { Tag = Tag.Parentheses; Content = ">" }

            yield { Tag = Tag.Parentheses; Content = "(" }
                    
            yield! constructObjectLiteral fl (false, ", ")
                    
            yield { Tag = Tag.Parentheses; Content = ")" }
            yield { Tag = Tag.Text; Content = ": " }

            yield! constructTypeDefinition tdef
            yield { Tag = Tag.EndOfLine; Content = ";" }
        ]

    | StructureStatement.ConstDefinition (ConstDefinition.DeclareConst ((Identifier i), tdef)) ->
        [
            yield { Tag = Tag.Keyword; Content = "declare const " }
            yield { Tag = Tag.Text; Content = i }
            yield { Tag = Tag.Text; Content = ": " }
            yield! constructTypeDefinition tdef
            yield { Tag = Tag.EndOfLine; Content = ";" }
        ]

    | StructureStatement.ConstDefinition (ConstDefinition.Const ((Identifier i), tdef)) ->
        [
            yield { Tag = Tag.Keyword; Content = "const " }
            yield { Tag = Tag.Text; Content = i }
            yield { Tag = Tag.Text; Content = ": " }
            yield! constructTypeDefinition tdef
            yield { Tag = Tag.EndOfLine; Content = ";" }
        ]


let toDocumentSegmentVmList statements =

    let rec interpret statements endTag result lastTag : DocumentSegmentViewModel list =

        /// append generated view models to the result and invokes interpret
        let continueInterpret tail xvm =
            interpret tail endTag (List.append result xvm)

        match statements with
        | head :: tail ->
            match head with
            | Statement.Import (entities, ``module``) ->
                let s =
                    if lastTag <> "import"  then
                        [
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            yield { Tag = Tag.Keyword; Content = "import " }
                        ]
                    else
                        [yield { Tag = Tag.Keyword; Content = "import " }]

                continueInterpret tail
                    [
                        yield! s
                        yield! (buildImportExportEntities (importEntity " ") entities [])
                        yield { Tag = Tag.Keyword; Content = "from " }
                        yield ``module`` |> modulePath
                        yield { Tag = Tag.EndOfLine; Content = ";" }
                    ]
                    "import"

            | Statement.Export (Transit (entities, ``module``)) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            yield { Tag = Tag.Modifier; Content = "export " }
                        ]
                    else
                        [yield { Tag = Tag.Modifier; Content = "export " }]

                continueInterpret tail
                    [
                        yield! s
                        yield! (buildImportExportEntities (exportEntity " ") entities [])
                        yield { Tag = Tag.Keyword; Content = "from " }
                        yield ``module`` |> modulePath
                        yield { Tag = Tag.EndOfLine; Content = ";" }
                    ]
                    "export"


            | Statement.Export (OutList entities) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            yield { Tag = Tag.Modifier; Content = "export " }
                        ]
                    else
                        [yield { Tag = Tag.Modifier; Content = "export " }]

                continueInterpret tail
                    [
                        yield! s
                        yield! (buildImportExportEntities (exportEntity "") (entities |> List.map (Named)) [])
                        yield { Tag = Tag.EndOfLine; Content = ";" }
                    ]
                    "export"


            | Statement.Export (OutDefault (Identifier i)) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            yield { Tag = Tag.Modifier; Content = "export " }
                        ]
                    else
                        [yield { Tag = Tag.Modifier; Content = "export " }]

                continueInterpret tail
                    [
                        yield! s
                        yield { Tag = Tag.Modifier; Content = "default " }
                        yield { Tag = Tag.Text; Content = i }
                        yield { Tag = Tag.EndOfLine; Content = ";" }
                    ]
                    "export"


            | Statement.Export (OutAssignment (Identifier identifier)) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            yield { Tag = Tag.Modifier; Content = "export" }
                        ]
                    else
                        [yield { Tag = Tag.Modifier; Content = "export" }]

                continueInterpret tail
                    [
                        yield! s
                        yield { Tag = Tag.Text; Content = " = " }
                        yield { Tag = Tag.Type; Content = identifier }
                        yield { Tag = Tag.EndOfLine; Content = ";" }
                    ]
                    "export"


            | Statement.Export (ExportStatement.Structure structure) ->

                let (breakeLine, lastTag') = 
                    match structure with
                    | FunctionDefinition _ -> (lastTag = "export function" |> not, "export function")
                    | _ -> (true, "")

                let xvm = interpretStructure structure

                continueInterpret tail
                    [
                        // each time insert break line
                        if breakeLine then
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            
                        yield { Tag = Tag.Modifier; Content = "export " }
                            
                        yield! xvm
                    ]
                    lastTag'

            | Statement.Export (ExportStatement.StructureDefault structure) ->
                
                    let (breakeLine, lastTag') = 
                        match structure with
                        | FunctionDefinition _ -> (lastTag = "export function" |> not, "export function")
                        | _ -> (true, "")
                
                    let xvm = interpretStructure structure
                
                    continueInterpret tail
                        [
                            // each time insert break line
                            if breakeLine then
                                yield { Tag = Tag.EndOfLine; Content = "" }
                                            
                            yield { Tag = Tag.Modifier; Content = "export " }
                            yield { Tag = Tag.Keyword; Content = "default " }
                                            
                            yield! xvm
                        ]
                        lastTag'

            | Statement.Export (ExportStatement.Namespace ((Identifier i), statements')) ->
                continueInterpret tail
                    [
                        yield { Tag = Tag.EndOfLine; Content = "" }
                        yield { Tag = Tag.Modifier; Content = "export " }
                        yield { Tag = Tag.Keyword; Content = "namespace " }
                        yield { Tag = Tag.Type; Content = i }
                        yield { Tag = Tag.Parentheses; Content = " {" }
                        yield { Tag = Tag.EndOfLine; Content = "" }
                        yield! interpret statements' (Tag.EndOfLine) [] ""
                        yield { Tag = Tag.Parentheses; Content = "}" }
                        yield { Tag = Tag.EndOfLine; Content = null }
                    ]
                    ""


            | Statement.Structure structure ->
                let xvm = interpretStructure structure
                continueInterpret tail
                    [
                        yield { Tag = Tag.EndOfLine; Content = "" }
                        yield! xvm
                    ]
                    ""

            | Statement.Comment comment ->
                let s =
                    if lastTag <> "comment"  then
                        [
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            yield { Tag = Tag.Comment; Content = comment }
                            yield { Tag = Tag.EndOfLine; Content = "" }
                        ]
                    else
                        [
                            yield { Tag = Tag.Comment; Content = comment }
                            yield { Tag = Tag.EndOfLine; Content = "" }
                        ]

                continueInterpret tail s "comment"

            | Statement.NamespaceDeclaration ((Identifier i), statements') ->
                continueInterpret tail
                    [
                        yield { Tag = Tag.EndOfLine; Content = "" }
                        yield { Tag = Tag.Keyword; Content = "declare namespace " }
                        yield { Tag = Tag.Type; Content = i }
                        yield { Tag = Tag.Parentheses; Content = " {" }
                        yield { Tag = Tag.EndOfLine; Content = "" }
                        yield! interpret statements' (Tag.EndOfLine) [] ""
                        yield { Tag = Tag.Parentheses; Content = "}" }
                        yield { Tag = Tag.EndOfLine; Content = null }
                    ]
                    ""

            | _ -> interpret tail endTag result ""
        | [] -> result @ [{ Tag = endTag; Content = null }]
                    
    (interpret statements (Tag.EndOfDocument) [] "").ToList()
