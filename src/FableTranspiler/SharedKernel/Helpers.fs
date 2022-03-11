﻿module FableTranspiler.Helpers

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
let toModuleName modifier (moduleRelativePath: string) =
    String.Join("",
        Path.GetFileName(moduleRelativePath)
            .Split("-")
            |> Seq.map modifier
    )