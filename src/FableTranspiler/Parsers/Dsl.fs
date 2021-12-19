module FableTranspiler.Parsers.Dsl

open Types
open System

let ``module`` path =
    let modulePath = ModulePath.Create path
    match Char.IsLetter(path.[0]) with
    | true -> DtsModule.NodeModule modulePath
    | false -> DtsModule.Relative modulePath


[<RequireQualifiedAccess>]
module Import =
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

    let ``default`` (path: string) =
        Statement.Import (
            [ImportEntity.No]
            , ``module`` path
        )



[<RequireQualifiedAccess>]
module Export =
    let outAssignment name =
        Statement.Export (Identifier.Create name |> OutAssignment)

    let outList names =
        Statement.Export (names |> List.map Identifier.Create |> OutList)


    let namedS name (path: string) =
        Statement.Export <| Transit (
            [ExportEntity.Named (name |> Identifier.Create)]
            , ``module`` path
        )

    let defaultAliasedS alias (path: string) =
        Statement.Export <| Transit (
            [ExportEntity.DefaultAliased (alias |> Identifier.Create)]
            , ``module`` path
        )

    /// <summary>
    /// 
    /// </summary>
    /// <param name="entities"> Choise1 - defaulted, Choice2 - named</param>
    /// <param name="path"></param>
    let transit (entities: Choice<string, string> list) (path: string) =
        Statement.Export <| Transit (
            entities
            |> List.map (function
                | Choice1Of2 name -> 
                    ExportEntity.DefaultAliased (name |> Identifier.Create)
                | Choice2Of2 name -> ExportEntity.Named (name |> Identifier.Create)
            )
            , ``module`` path
        )



[<RequireQualifiedAccess>]
module Comment =
    let create comment =
        Statement.Comment comment