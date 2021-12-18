module FableTranspiler.Parsers.Dsl

open Types
open System

[<RequireQualifiedAccess>]
module Import =
    let ``module`` path =
        let modulePath = ModulePath.Create path
        match Char.IsLetter(path.[0]) with
        | true -> ImportModule.NodeModule modulePath
        | false -> ImportModule.Relative modulePath


    let allAliased alias (path: string) =
        Statement.Import (
            [ImportEntity.AllAliased (alias |> Identifier.Create)]
            , ``module`` path
        )

    let named (names: string list) (path: string) =
        Statement.Import (
            names |> List.map (Identifier.Create >> ImportEntity.Named)
            , ``module`` path
        )

    let namedS (name: string) (path: string) =
        Statement.Import (
            name |> (Identifier.Create >> ImportEntity.Named >> List.singleton)
            , ``module`` path
        )

    let aliasedS (name: string) (alias: string) (path: string) =
        Statement.Import (
            (name |> Identifier.Create, alias |> Identifier.Create)
            |> ImportEntity.Aliased 
            |> List.singleton
            , ``module`` path
        )

    let aliased (nameAlias: (string * string) list) (path: string) =
        Statement.Import (
            nameAlias
            |> List.map (fun t -> 
                let name, alias = t
                (name |> Identifier.Create, alias |> Identifier.Create)
                |> ImportEntity.Aliased 
            )
            , ``module`` path
        )
