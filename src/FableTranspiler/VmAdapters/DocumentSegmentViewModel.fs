namespace FableTranspiler.VmAdapters

open FableTranspiler.Parsers.Types
open FableTranspiler.Parsers.Dsl
open System.Windows
open System.Windows.Documents
open System
open System.Linq

type DocumentSegmentViewModel =
    {
        Tag: Tag
        Content: string
    }
and
    Tag =
        | NoContent = 0
        | Keyword = 1
        | Text = 2
        | EndOfLine = 3
        | EndOfDocument = 4
        | Comment = 5
        | Type = 6
        | Modifier = 7
        | Parentheses = 8


module internal rec DocumentSegment =

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


    let rec private constructObjectLiteral (fields: (Field * TypeDefinition) list) endLine =
        fields
        |> List.map (fun l ->
            let (fld, tdef) = l
            [
                if endLine then
                    yield { Tag = Tag.Text; Content = "    " }

                match fld with
                | Required (Identifier i) ->
                    yield { Tag = Tag.Text; Content = i }
                    yield { Tag = Tag.Text; Content = ": " }
                | Optional (Identifier i) ->
                    yield { Tag = Tag.Text; Content = i }
                    yield { Tag = Tag.Text; Content = "?: " }
                | FuncOpt (Identifier i, fl) ->
                    yield { Tag = Tag.Text; Content = i }
                    yield { Tag = Tag.Text; Content = "?(" }
                    yield! constructObjectLiteral fl false
                    yield { Tag = Tag.Text; Content = "): " }

                match tdef with
                | TypeDefinition.Single tn -> 
                    yield! constructSingleType tn
                | TypeDefinition.Combination comb ->
                    match comb with
                    | Union l ->
                        yield! constructCombination " | " l []
                    | Composition l ->
                        yield!  constructCombination " & " l [] 



                if endLine then
                    yield { Tag = Tag.EndOfLine; Content = ";" }
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
                    { Tag = Tag.Text; Content = sep }
                    yield! t2
                ]
            )
        | head :: tail ->
            match head with
            | TypeName.Plain p -> 
                constructType p
                |> (fun r -> constructCombination sep tail (r :: res))
                    
            | TypeName.Generic (p, i) ->
                let main = constructType p
                let inner = constructCombination ", " i []
                let l =
                    [
                        yield! main
                        { Tag = Tag.Text; Content = "<" }
                        yield! inner
                        { Tag = Tag.Text; Content = "> " }
                    ]
                constructCombination sep tail (l :: res)

            | TypeName.Undefined -> 
                let l =
                    [
                        { Tag = Tag.Type; Content = "undefined" }
                    ]
                constructCombination sep tail (l :: res)

            | TypeName.Void -> 
                let l =
                    [
                        { Tag = Tag.Type; Content = "void" }
                    ]
                constructCombination sep tail (l :: res)

            | TypeName.Func (fl, tdef) ->
                let l =
                    [
                        { Tag = Tag.Text; Content = "((" }
                        yield! constructObjectLiteral fl false
                        { Tag = Tag.Text; Content = ") => " }

                        yield! constructTypeDefinition tdef
                        { Tag = Tag.Text; Content = ")" }
                    ]
                constructCombination sep tail (l :: res)





    let rec private interpretStructure structure : DocumentSegmentViewModel list =
        match structure with
        | TypeAlias (Identifier identifier, combination) ->
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
                    
                yield! constructObjectLiteral lit true
                    
                yield { Tag = Tag.Parentheses; Content = "}" }
                yield { Tag = Tag.EndOfLine; Content = null }
            ]

        | InterfaceDefinition (Plain (Identifier idetifier, lit)) ->
            [
                yield { Tag = Tag.Keyword; Content = "interface " }
                yield { Tag = Tag.Type; Content = idetifier }

                yield { Tag = Tag.Parentheses; Content = "{" }
                yield { Tag = Tag.EndOfLine; Content = null }
                    
                yield! constructObjectLiteral lit true
                    
                yield { Tag = Tag.Parentheses; Content = "}" }
                yield { Tag = Tag.EndOfLine; Content = null }
            ]


    let toDocumentSegmentVmList statements =

        let rec interpret statements result lastTag : DocumentSegmentViewModel list =

            /// append generated view models to the result and invokes interpret
            let continueInterpret tail xvm =
                interpret tail (List.append result xvm)

            match statements with
            | head :: tail ->
                match head with
                | Import (entities, ``module``) ->
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

                | Export (Transit (entities, ``module``)) ->
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


                | Export (OutList entities) ->
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


                | Export (OutAssignment (Identifier identifier)) ->
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


                | Export (ExportStatement.Structure structure) ->
                    let xvm = interpretStructure structure

                    continueInterpret tail
                        [
                            // each time insert break line
                            yield { Tag = Tag.EndOfLine; Content = "" }
                            
                            match structure with
                            | ClassDefinition _ ->
                                yield { Tag = Tag.Modifier; Content = "export " }
                                yield { Tag = Tag.Keyword; Content = "default " }
                            | _ ->
                                yield { Tag = Tag.Modifier; Content = "export " }
                            
                            yield! xvm
                        ]
                        ""

                | Comment comment ->
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

                | _ -> interpret tail result ""
            | [] -> result @ [{ Tag = Tag.EndOfDocument; Content = null }]
                    
        (interpret statements [] "").ToList()



    let fakeModule () =
        [
            Import.allAliased "React" "react"
            Import.namedS "ReactScrollLinkProps" "./mixins/scroller"
        ]

