namespace FableTranspiler.VmAdapters.Types




[<StructuralEquality; StructuralComparison>]
type CodeItem =
    {
        Tag: Tag
        Content: string
    }
    with
        member this.GetContent() =
            this.Content
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



[<ReferenceEquality>]
type DtsStatementDto =
    {
        Index: int
        DtsStatement: FableTranspiler.Parsers.Types.Statement
        DtsDocumentSection: CodeItem list
    }


type TabLevel = TabLevel of int with
    static member (+) (TabLevel tabLevel, v) =
        (tabLevel + v) |> TabLevel

    static member (+) (v, TabLevel tabLevel) =
        (tabLevel + v) |> TabLevel


[<AutoOpen>]
module internal CodeItem =
    open FableTranspiler.Parsers.Types
    
    let createDtsVm dtsStatement ind dtsDocumentSection =
        {
            Index = ind
            DtsStatement = dtsStatement
            DtsDocumentSection = dtsDocumentSection
        }

    let vm tag content =
        { Tag = tag; Content = content }

    let vmKeyword content =
        { Tag = Tag.Keyword; Content = content }

    let vmModifier content =
        { Tag = Tag.Modifier; Content = content }

    let vmComment content =
        { Tag = Tag.Comment; Content = content }

    let vmType content =
        { Tag = Tag.Type; Content = content }

    let vmTypeS content =
        { Tag = Tag.Type; Content = $"{content} " }

    /// adds space to the end
    let vmKeywordS content =
        { Tag = Tag.Keyword; Content = $"{content} " }

    let vmText content =
        { Tag = Tag.Text; Content = content }

    /// adds space to the end
    let vmTextS content =
        { Tag = Tag.Text; Content = $"{content} " }

    let vmEndLine content =
        { Tag = Tag.EndOfLine; Content = content }

    let vmEndLineNull =
        { Tag = Tag.EndOfLine; Content = null }

    /// Tag.Parentheses
    let vmPrn content =
        { Tag = Tag.Parentheses; Content = content }

    let tab (TabLevel tabLevel) =
        { Tag = Tag.Text; Content = String.replicate (4 * tabLevel) " " }

    let vmIdentifier (Identifier identifier) =
        { Tag = Tag.Text; Content = identifier }

    let vmNo =
        { Tag = Tag.NoContent; Content = null }