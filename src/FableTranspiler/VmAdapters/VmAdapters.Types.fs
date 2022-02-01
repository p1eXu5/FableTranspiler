namespace FableTranspiler.VmAdapters.Types

open FableTranspiler.Parsers.Types
open System




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


type InterfaceFsStatement =
    {
        Name: string
        Content: CodeItem list
        Construct: CodeItem list
    }

[<RequireQualifiedAccess>]
type FsStatement =
    | Nameless of CodeItem list
    | Named of Name: string * CodeItem list
    | Link of Name: string * FsStatement
    | Let of Name: string * CodeItem list * TypeConstructor: (unit -> CodeItem list)
    | Typed of Name: string * CodeItem list * TypeConstructor: CodeItem list
    | Interface of InterfaceFsStatement


type GetFsStatement = string -> FsStatement option

type internal FsStatementStore =
    {
        Get: string -> GetFsStatement
        Add: string -> string -> FsStatement -> unit
    }



[<ReferenceEquality>]
type DtsStatementDto =
    {
        Index: int
        DtsStatement: FableTranspiler.Parsers.Types.Statement
        DtsDocumentSection: CodeItem list
    }



type FsCodeStyle =
    | Universal
    | Fable
    | React
    | Feliz


[<ReferenceEquality>]
type FsStatementDto =
    {
        Index: int
        DtsStatement: FableTranspiler.Parsers.Types.Statement option
        StyledFsStatements: StyledFsStatement list
    }
and
    StyledFsStatement =
        {
            FsCodeStyle: FsCodeStyle
            FsStatement: FsStatement
        }



type internal Interpreters =
    {
        InterpretPlainFableInterface: GetFsStatement -> int -> string -> FieldList -> (CodeItem list * CodeItem list)
    }


[<RequireQualifiedAccess>]
module internal FsStatement =

    let name = function
        | FsStatement.Nameless _ -> None
        | FsStatement.Named (n, _) -> n |> Some
        | FsStatement.Link (n, _) -> n |> Some
        | FsStatement.Let (n, _, _) -> n |> Some
        | FsStatement.Typed (n, _, _) -> n |> Some
        | FsStatement.Interface v -> v.Name |> Some

    let rec segments = function
        | FsStatement.Nameless l -> l
        | FsStatement.Named (_, l) -> l
        | FsStatement.Link (_, l) -> l |> segments
        | FsStatement.Let (_, l, _) -> l
        | FsStatement.Typed (_, l, _) -> l
        | FsStatement.Interface v -> v.Content

    let rec construct = function
        | FsStatement.Nameless _ -> None
        | FsStatement.Named (_, l) -> l |> Some
        | FsStatement.Link (_, l) -> l |> construct
        | FsStatement.Let (_, _, f) -> f() |> Some
        | FsStatement.Typed (_, _, l) -> l |> Some
        | FsStatement.Interface v -> v.Construct |> Some

    let rec insertAtEnd segment = function
        | FsStatement.Nameless l -> l @ [segment] |> FsStatement.Nameless
        | FsStatement.Named (name, l) -> (name, l @ [segment]) |> FsStatement.Named
        | FsStatement.Link (name, l) -> (name, insertAtEnd segment l) |> FsStatement.Link
        | FsStatement.Let (name, l, constructor) -> (name, l @ [segment], constructor) |> FsStatement.Let
        | FsStatement.Typed (name, l, constructor) -> (name, l @ [segment], constructor) |> FsStatement.Typed
        | FsStatement.Interface v ->
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
module internal FsStatementDto =

    let createFsVm dtsStatement ind codeStyle fsStatement =
        {
            Index = ind
            DtsStatement = dtsStatement
            StyledFsStatements = 
                [{
                    FsCodeStyle = codeStyle
                    FsStatement = fsStatement
                }]
        }

type FsStatementDto with
    member this.Content() =
        raise (NotImplementedException())
        //this.StyledFsStatements[this.SelectedFsStatement]
        //    .FsStatement
        //    |> FsStatement.segments

    member this.FsCodeStyle() =
        raise (NotImplementedException())
        //this.StyledFsStatements[this.SelectedFsStatement]
        //    .FsCodeStyle



[<AutoOpen>]
module internal CodeItem =
    
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

    let tab level =
        { Tag = Tag.Text; Content = String.replicate (4 * level) " " }

    let vmNo =
        { Tag = Tag.NoContent; Content = null }