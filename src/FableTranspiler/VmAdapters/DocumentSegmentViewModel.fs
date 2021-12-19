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


module internal DocumentSegment =

    type EntityOrder =
        | Single
        | First
        | Last
        | Middle

    let toDocumentSegmentVmList statements =

        let modulePath = function
            | NodeModule (ModulePath path) -> { Tag = Tag.Text; Content = sprintf "'%s'" path}
            | Relative (ModulePath path) -> { Tag = Tag.Text; Content = sprintf "'%s'" path}

        

        let insertWithOrder entityOrder posfix content =
            match entityOrder with
            | Single -> sprintf "{ %s }%s" content posfix
            | First -> sprintf "{ %s, " content
            | Last -> sprintf "%s }%s" content posfix
            | Middle -> sprintf "%s, " content

        let insertAsWithOrder entityOrder nameTag name alias postfix =
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

        
        let importEntity postfix entity entityOrder = 
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


        let exportEntity postfix entity entityOrder = 
            match entity with
            | ExportEntity.Named (Identifier identifier) -> [{ Tag = Tag.Text; Content = identifier |> insertWithOrder entityOrder postfix}]
            | DefaultAliased (Identifier alias) -> insertAsWithOrder entityOrder Tag.Modifier "default" alias postfix




        let rec buildEntities builder l res =
            match l with
            | head::[] when res |> List.isEmpty -> 
                builder head Single

            | head::[] when res |> List.isEmpty |> not -> 
                res @ builder head Last

            | head::tail when res |> List.isEmpty ->
                let res' = res @ builder head First
                buildEntities builder tail res'

            | head::tail when res |> List.isEmpty |> not ->
                let res' = res @ builder head Middle
                buildEntities builder tail res'

            | _ -> []
                


        let rec interpret statements result lastTag =

            let continueInterpret tail s =
                interpret tail (List.append result s)

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
                            yield! (buildEntities (importEntity " ") entities [])
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
                            yield! (buildEntities (exportEntity " ") entities [])
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
                            yield! (buildEntities (exportEntity "") (entities |> List.map (Named)) [])
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

