﻿namespace FableTranspiler.VmAdapters



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


type FsDocumentSegmentListViewModel =
    | Nameless of DocumentSegmentViewModel list
    | Let of Name: string * DocumentSegmentViewModel list * TypeConstructor: (unit -> DocumentSegmentViewModel list)
    | Typed of Name: string * DocumentSegmentViewModel list * TypeConstructor: DocumentSegmentViewModel list




module internal DocumentSegmentViewModel =
    let name = function
        | Nameless _ -> failwith "No name"
        | Let (n, _, _) -> n
        | Typed (n, _, _) -> n

    let segments = function
        | Nameless l -> l
        | Let (_, l, _) -> l
        | Typed (_, l, _) -> l

    let construct = function
        | Nameless _ -> failwith "No type constructor"
        | Let (_, _, f) -> f()
        | Typed (_, _, l) -> l

    let vm tag content =
        { Tag = tag; Content = content }

    let vmKeyword content =
        { Tag = Tag.Keyword; Content = content }

    let vmType content =
        { Tag = Tag.Type; Content = content }

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