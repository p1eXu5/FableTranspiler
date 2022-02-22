namespace FableTranspiler.Interpreters


[<StructuralEquality; StructuralComparison>]
type CodeItem =
    {
        Tag: Tag
        Content: string
    }
    with
        member this.GetContent() =
            this.Content
        override this.ToString() =
            match this.Tag with
            | Tag.EndOfLine when this.Content = null -> System.Environment.NewLine
            | Tag.EndOfLine -> this.Content + System.Environment.NewLine
            | _ -> this.Content
and
    Tag =
        | NoContent = 0
        | Keyword = 1
        | Text = 2
        | Comment = 3
        | Type = 4
        | Modifier = 5
        | Parentheses = 6
        | EndOfLine = 7


type TabLevel = TabLevel of int with
    static member (+) (TabLevel tabLevel, v) =
        (tabLevel + v) |> TabLevel

    static member (+) (v, TabLevel tabLevel) =
        (tabLevel + v) |> TabLevel


[<AutoOpen>]
module CodeItem =
    open FableTranspiler.SimpleTypes

    let internal vm tag content =
        { Tag = tag; Content = content }

    let internal vmKeyword content =
        { Tag = Tag.Keyword; Content = content }

    let internal vmModifier content =
        { Tag = Tag.Modifier; Content = content }

    let internal vmComment content =
        { Tag = Tag.Comment; Content = content }

    let internal vmType content =
        { Tag = Tag.Type; Content = content }

    let internal vmTypeS content =
        { Tag = Tag.Type; Content = $"{content} " }

    /// adds space to the end
    let internal vmKeywordS content =
        { Tag = Tag.Keyword; Content = $"{content} " }

    let internal vmText content =
        { Tag = Tag.Text; Content = content }

    /// adds space to the end
    let internal vmTextS content =
        { Tag = Tag.Text; Content = $"{content} " }

    let internal vmEndLine content =
        { Tag = Tag.EndOfLine; Content = content }

    let internal vmEndLineNull =
        { Tag = Tag.EndOfLine; Content = null }

    /// Tag.Parentheses
    let internal vmPrn content =
        { Tag = Tag.Parentheses; Content = content }

    let internal tab (TabLevel tabLevel) =
        { Tag = Tag.Text; Content = String.replicate (4 * tabLevel) " " }

    let internal vmIdentifier (Identifier identifier) =
        { Tag = Tag.Text; Content = identifier }

    let internal vmNo =
        { Tag = Tag.NoContent; Content = null }

    let interpretError (err: string) =
        (err.Split(System.Environment.NewLine)
        |> Array.toList
        |> List.map (fun s ->
            [
                vmText s
                vmEndLineNull
            ]
        )
        |> List.concat)