namespace FableTranspiler.VmAdapters


[<ReferenceEqualityAttribute>]
type DocumentSegmentViewModel =
    {
        Tag: Tag
        Content: Content
    }
    with
        member this.GetContent() =
            match this.Content with
            | No -> null
            | Text t -> t
            | Statement s -> null
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
        | EndOfStatement = 8
        | EndOfDocument = 9
and
    Content =
        | No
        | Text of string
        | Statement of ref<FableTranspiler.Parsers.Types.Statement>


type FsDocumentSegmentListViewModel =
    | Nameless of DocumentSegmentViewModel list
    | Named of Name: string * DocumentSegmentViewModel list
    | Link of Name: string * FsDocumentSegmentListViewModel
    | Let of Name: string * DocumentSegmentViewModel list * TypeConstructor: (unit -> DocumentSegmentViewModel list)
    | Typed of Name: string * DocumentSegmentViewModel list * TypeConstructor: DocumentSegmentViewModel list



module internal FsDocumentSegmentListViewModel =

    let name = function
        | Nameless _ -> failwith "No name"
        | Named (n, _) -> n
        | Link (n, _) -> n
        | Let (n, _, _) -> n
        | Typed (n, _, _) -> n

    let rec segments = function
        | Nameless l -> l
        | Named (_, l) -> l
        | Link (_, l) -> l |> segments
        | Let (_, l, _) -> l
        | Typed (_, l, _) -> l

    let rec construct = function
        | Nameless _ -> failwith "No type constructor"
        | Named (_, l) -> l
        | Link (_, l) -> l |> construct
        | Let (_, _, f) -> f()
        | Typed (_, _, l) -> l

    let rec insertAtEnd segment = function
        | Nameless l -> l @ [segment] |> Nameless
        | Named (name, l) -> (name, l @ [segment]) |> Named
        | Link (name, l) -> (name, insertAtEnd segment l) |> Link
        | Let (name, l, constructor) -> (name, l @ [segment], constructor) |> Let
        | Typed (name, l, constructor) -> (name, l @ [segment], constructor) |> Typed


module internal DocumentSegmentViewModel =
    



    let vm tag content =
        { Tag = tag; Content = content |> Content.Text }

    let vmKeyword content =
        { Tag = Tag.Keyword; Content = content |> Content.Text }

    let vmModifier content =
        { Tag = Tag.Modifier; Content = content |> Content.Text }

    let vmComment content =
        { Tag = Tag.Comment; Content = content |> Content.Text }

    let vmType content =
        { Tag = Tag.Type; Content = content |> Content.Text }

    let vmTypeS content =
        { Tag = Tag.Type; Content = $"{content} " |> Content.Text }

    /// adds space to the end
    let vmKeywordS content =
        { Tag = Tag.Keyword; Content = $"{content} " |> Content.Text }

    let vmText content =
        { Tag = Tag.Text; Content = content |> Content.Text }

    /// adds space to the end
    let vmTextS content =
        { Tag = Tag.Text; Content = $"{content} " |> Content.Text }

    let vmEndLine content =
        { Tag = Tag.EndOfLine; Content = content |> Content.Text }

    let vmEndLineNull =
        { Tag = Tag.EndOfLine; Content = Content.No }

    let vmEndDocument =
        { Tag = Tag.EndOfDocument; Content = Content.No }

    let vmEndStatement s =
        { Tag = Tag.EndOfStatement; Content = s |> Content.Statement }

    /// Tag.Parentheses
    let vmPrn content =
        { Tag = Tag.Parentheses; Content = content  |> Content.Text}

    let tab level =
        { Tag = Tag.Text; Content = String.replicate (4 * level) " " |> Content.Text }

    let vmNo =
        { Tag = Tag.NoContent; Content = Content.No }