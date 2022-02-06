module FableTranspiler.SimpleTypes

open ConstrainedTypes


type Name = Name of string with
    static member Create(v) = ConstrainedString.Create(nameof Name, Name, 1, 1024, v)
    static member Value(Name v) = v


type LibLocation = LibLocation of string with
    static member Create(v) = ConstrainedString.Create(nameof LibLocation, LibLocation, 3, 1024, v)
    static member Value(LibLocation v) = v


type ModulePath = ModulePath of string with
    static member Create(v) = ConstrainedString.Create(nameof ModulePath, ModulePath, 3, 1024, v)
    static member Value(ModulePath v) = v
