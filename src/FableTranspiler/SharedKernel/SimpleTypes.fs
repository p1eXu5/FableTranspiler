module FableTranspiler.SimpleTypes

open ConstrainedTypes


type Identifier = Identifier of string with
    static member Create(v) = Identifier v
    static member Value(Identifier v) = v


type LibLocation = LibLocation of string with
    static member Create(v) = ConstrainedString.Create(nameof LibLocation, LibLocation, 3, 1024, v)
    static member Value(LibLocation v) = v


type ModulePath = ModulePath of string with
    static member Create(v) = ConstrainedString.Create(nameof ModulePath, ModulePath, 3, 1024, v)
    static member Value(ModulePath v) = v


open System.IO

[<Struct>]
type FullPath = private FullPath of string with
    static member Create(v: string) =
        if Path.IsPathFullyQualified(v) then
            FullPath v |> Ok
        else
            Error $"{v} is not full path"

    static member Value(FullPath v) = v