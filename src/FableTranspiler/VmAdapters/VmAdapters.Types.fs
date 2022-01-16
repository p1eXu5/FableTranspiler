namespace FableTranspiler.VmAdapters

open FableTranspiler.Parsers.Types




[<StructuralEquality; StructuralComparison>]
type CodeItemViewModel =
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


type FsStatement =
    | Nameless of CodeItemViewModel list
    | Named of Name: string * CodeItemViewModel list
    | Link of Name: string * FsStatement
    | Let of Name: string * CodeItemViewModel list * TypeConstructor: (unit -> CodeItemViewModel list)
    | Typed of Name: string * CodeItemViewModel list * TypeConstructor: CodeItemViewModel list


type GetFsStatement = string -> FsStatement option

type FsStatementStore =
    {
        Get: string -> GetFsStatement
        Add: string -> string -> FsStatement -> unit
    }



[<ReferenceEquality>]
type DtsStatementViewModel =
    {
        DtsStatement: FableTranspiler.Parsers.Types.Statement
        DtsDocumentSection: CodeItemViewModel list
    }


[<ReferenceEquality>]
type FsStatementViewModel =
    {
        DtsStatement: FableTranspiler.Parsers.Types.Statement option
        FsStatement: FsStatement
        FsCodeStyle: FsCodeStyle
    }
and
    FsCodeStyle =
        | Universal
        | Fable
        | React
        | Feliz



type Interpreters =
    {
        InterpretPlainFableInterface: GetFsStatement -> int -> string -> FieldList -> (CodeItemViewModel list * CodeItemViewModel list)
    }


[<RequireQualifiedAccess>]
module internal FsStatement =

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


type FsStatement with
    member this.Content() = FsStatement.segments this


[<AutoOpen>]
module internal CodeItemViewModel =
    
    let createDtsVm dtsStatement dtsDocumentSection =
        {
            DtsStatement = dtsStatement
            DtsDocumentSection = dtsDocumentSection
        }

    let createFsVm dtsStatement codeStyle fsDocumentSection =
        {
            DtsStatement = dtsStatement
            FsStatement = fsDocumentSection
            FsCodeStyle = codeStyle
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

    let tab level =
        { Tag = Tag.Text; Content = String.replicate (4 * level) " " }

    let vmNo =
        { Tag = Tag.NoContent; Content = null }