namespace FableTranspiler

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


module internal Implementation =

    type EntityOrder =
        | Single
        | First
        | Last
        | Middle

    let flowDocumentInterpretator statements =

        let modulePath = function
            | NodeModule (ModulePath path) -> { Tag = Tag.Text; Content = sprintf "'%s'" path}
            | Relative (ModulePath path) -> { Tag = Tag.Text; Content = sprintf "'%s'" path}

        
        
        let importEntity entity entityOrder = 

            let insert content =
                match entityOrder with
                | Single -> sprintf "{%s} " content
                | First -> sprintf "{%s, " content
                | Last -> sprintf "%s} " content
                | Middle -> sprintf "%s, " content

            match entity with
            | No -> [{ Tag = Tag.NoContent; Content = null }]
            | ImportEntity.Named (Identifier identifier) -> [{ Tag = Tag.Text; Content = identifier |> insert}]
            | Aliased (Identifier name, Identifier alias) -> 
                match entityOrder with
                | First ->
                    [
                        { Tag = Tag.Text; Content = "{"}
                        { Tag = Tag.Text; Content = name }
                        { Tag = Tag.Keyword; Content = " as "}
                        { Tag = Tag.Type; Content = alias}
                        { Tag = Tag.Text; Content = ", "}
                    ]
                | Last ->
                    [
                        { Tag = Tag.Text; Content = name }
                        { Tag = Tag.Keyword; Content = " as "}
                        { Tag = Tag.Type; Content = alias}
                        { Tag = Tag.Text; Content = "} "}
                    ]
                | Middle ->
                    [
                        { Tag = Tag.Text; Content = name }
                        { Tag = Tag.Keyword; Content = " as "}
                        { Tag = Tag.Type; Content = alias}
                        { Tag = Tag.Text; Content = ", "}
                    ]
                | Single ->
                    [
                        { Tag = Tag.Text; Content = "{"}
                        { Tag = Tag.Text; Content = name }
                        { Tag = Tag.Keyword; Content = " as "}
                        { Tag = Tag.Type; Content = alias}
                        { Tag = Tag.Text; Content = "}"}
                    ]

            | All -> [{ Tag = Tag.Text; Content = "* "}]
            | AllAliased (Identifier alias) -> 
                [
                    { Tag = Tag.Text; Content = "* "}
                    { Tag = Tag.Keyword; Content = "as "}
                    { Tag = Tag.Type; Content = sprintf "%s " alias}
                ]



        let rec buildEntities l res =
            match l with
            | head::[] when res |> List.isEmpty -> 
                importEntity head Single

            | head::[] when res |> List.isEmpty |> not -> 
                importEntity head Last @ res

            | head::tail when res |> List.isEmpty ->
                let res' = importEntity head First @ res
                buildEntities tail res'

            | head::tail when res |> List.isEmpty |> not ->
                let res' = importEntity head Middle @ res
                buildEntities tail res'

            | _ -> []
                


        let rec interpret statements result =

            let continueInterpret tail s =
                interpret tail (List.append result s)

            match statements with
            | head :: tail ->
                match head with
                | Import (entities, ``module``) ->
                    continueInterpret tail
                        [
                            yield { Tag = Tag.Keyword; Content = "import " }
                            yield! (buildEntities entities [])
                            yield { Tag = Tag.Keyword; Content = "from " }
                            yield ``module`` |> modulePath
                            yield { Tag = Tag.EndOfLine; Content = ";" }
                        ]

                | Export (OutAssignment (Identifier identifier)) ->
                    continueInterpret tail
                        [
                            yield { Tag = Tag.Keyword; Content = "export" }
                            yield { Tag = Tag.Text; Content = " = " }
                            yield { Tag = Tag.Type; Content = identifier }
                            yield { Tag = Tag.EndOfLine; Content = ";" }
                        ]

                | Comment comment ->
                    continueInterpret tail
                        [
                            yield { Tag = Tag.Comment; Content = comment }
                            yield { Tag = Tag.EndOfLine; Content = "" }
                        ]

                | _ -> interpret tail result
            | [] -> result @ [{ Tag = Tag.EndOfDocument; Content = null }]
                    
        (interpret statements []).ToList()


    let fakeModule () =
        [
            Import.allAliased "React" "react"
            Import.namedS "ReactScrollLinkProps" "./mixins/scroller"
        ]

