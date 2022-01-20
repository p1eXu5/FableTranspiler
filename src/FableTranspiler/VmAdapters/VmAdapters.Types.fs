namespace FableTranspiler.VmAdapters

open FableTranspiler.Parsers.Types
open System




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


type InterfaceFsStatement =
    {
        Name: string
        Content: CodeItemViewModel list
        Construct: CodeItemViewModel list
    }


type FsStatement =
    | Nameless of CodeItemViewModel list
    | Named of Name: string * CodeItemViewModel list
    | Link of Name: string * FsStatement
    | Let of Name: string * CodeItemViewModel list * TypeConstructor: (unit -> CodeItemViewModel list)
    | Typed of Name: string * CodeItemViewModel list * TypeConstructor: CodeItemViewModel list
    | Interface of InterfaceFsStatement


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



type FsCodeStyle =
    | Universal
    | Fable
    | React
    | Feliz


[<ReferenceEquality>]
type FsStatementViewModel =
    {
        DtsStatement: FableTranspiler.Parsers.Types.Statement option
        FsStatement: (FsCodeStyle * FsStatement) list
        IsMuted: bool
    }



type Interpreters =
    {
        InterpretPlainFableInterface: GetFsStatement -> int -> string -> FieldList -> (CodeItemViewModel list * CodeItemViewModel list)
    }


[<RequireQualifiedAccess>]
module internal FsStatement =

    let name = function
        | Nameless _ -> None
        | Named (n, _) -> n |> Some
        | Link (n, _) -> n |> Some
        | Let (n, _, _) -> n |> Some
        | Typed (n, _, _) -> n |> Some
        | Interface v -> v.Name |> Some

    let rec segments = function
        | Nameless l -> l
        | Named (_, l) -> l
        | Link (_, l) -> l |> segments
        | Let (_, l, _) -> l
        | Typed (_, l, _) -> l
        | Interface v -> v.Content

    let rec construct = function
        | Nameless _ -> None
        | Named (_, l) -> l |> Some
        | Link (_, l) -> l |> construct
        | Let (_, _, f) -> f() |> Some
        | Typed (_, _, l) -> l |> Some
        | Interface v -> v.Construct |> Some

    let rec insertAtEnd segment = function
        | Nameless l -> l @ [segment] |> Nameless
        | Named (name, l) -> (name, l @ [segment]) |> Named
        | Link (name, l) -> (name, insertAtEnd segment l) |> Link
        | Let (name, l, constructor) -> (name, l @ [segment], constructor) |> Let
        | Typed (name, l, constructor) -> (name, l @ [segment], constructor) |> Typed
        | Interface v ->
            {
                v with
                    Content = v.Content @ [segment]
            }
            |> FsStatement.Interface


type FsStatement with
    member this.Content() = FsStatement.segments this
    member this.Name() = FsStatement.name this
    member this.StringContent() =
        this.Content()
        |> List.map (fun ci ->
            match ci.Tag with
            | Tag.NoContent -> ""
            | Tag.EndOfLine -> "\n"
            | _ -> ci.Content
        )
        |> fun l -> String.Join("", l)
    member this.Construct() =
        match FsStatement.construct this with
        | Some l -> l
        | None -> []


[<AutoOpen>]
module internal CodeItemViewModel =
    
    let createDtsVm dtsStatement dtsDocumentSection =
        {
            DtsStatement = dtsStatement
            DtsDocumentSection = dtsDocumentSection
        }

    let createFsVm dtsStatement codeStyle fsStatement =
        {
            DtsStatement = dtsStatement
            FsStatement = [(codeStyle, fsStatement)]
            IsMuted = false
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