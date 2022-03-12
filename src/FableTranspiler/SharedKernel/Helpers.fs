module FableTranspiler.Helpers

open SimpleTypes
open System
open System.IO


let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None 

let capitalizeFirstLetter (s: string) =
    Char.ToUpperInvariant(s[0]).ToString() + s[1..]

let uncapitalizeFirstLetter (s: string) =
    Char.ToLowerInvariant(s[0]).ToString() + s[1..]

/// Examples of modifiers: Helpers.capitalizeFirstLetter and Helpers.uncapitalizeFirstLetter
let toModuleName nameStyle (moduleRelativePath: string) =
    let nameSegments =
        Path.GetFileName(moduleRelativePath)
            .Split("-")
            |> Seq.map capitalizeFirstLetter
            |> Seq.toList
        
    String.Join("",
        match nameStyle with
        | CamelCase ->
            (nameSegments |> List.head |> uncapitalizeFirstLetter) :: (nameSegments |> List.tail)
        | PascalCase -> nameSegments
    )


module List =

    let inline skipLast<'a> (l: 'a list) : 'a list =
        if l.Length = 0 then []
        else l |> List.take (l.Length - 1)

    /// <summary>
    /// </summary>
    /// <param name="l"></param>
    /// <exception> <see cref="InvalidOperationException"/> </exception>
    let inline partitionLast<'a> (l: 'a list) : 'a list * 'a =
        if l.Length = 0 then raise (InvalidOperationException("List is empty"))
        else (l |> List.take (l.Length - 1), l |> List.last)