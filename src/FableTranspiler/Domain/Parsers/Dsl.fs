module FableTranspiler.Parsers.Dsl

open Types
open System
open FableTranspiler.SimpleTypes

let ``module`` path =
    match (ModulePath.Create path), (Char.IsLetter(path.[0])) with
    | (Ok modulePath), true -> DtsModule.NodeModule modulePath
    | (Ok modulePath), false -> DtsModule.Relative modulePath
    | Error err, _ -> failwith err


[<RequireQualifiedAccess>]
module Import =
    let allAliased alias (path: string) =
        Statement.Import (
            [ImportEntity.AllAliased (alias |> Identifier.create)]
            , ``module`` path
        )

    let named (names: string list) (path: string) =
        Statement.Import (
            names |> List.map (Identifier.create >> ImportEntity.Named)
            , ``module`` path
        )

    let namedS (name: string) (path: string) =
        Statement.Import (
            name |> (Identifier.create >> ImportEntity.Named >> List.singleton)
            , ``module`` path
        )

    let aliasedS (name: string) (alias: string) (path: string) =
        Statement.Import (
            (name |> Identifier.create, alias |> Identifier.create)
            |> ImportEntity.Aliased 
            |> List.singleton
            , ``module`` path
        )

    let aliased (nameAlias: (string * string) list) (path: string) =
        Statement.Import (
            nameAlias
            |> List.map (fun t -> 
                let name, alias = t
                (name |> Identifier.create, alias |> Identifier.create)
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
        Statement.Export (Identifier.create name |> OutAssignment)

    let outList names =
        Statement.Export (names |> List.map Identifier.create |> OutList)


    let namedS name (path: string) =
        Statement.Export <| Transit (
            [ExportEntity.Named (name |> Identifier.create)]
            , ``module`` path
        )

    let defaultAliasedS alias (path: string) =
        Statement.Export <| Transit (
            [ExportEntity.DefaultAliased (alias |> Identifier.create)]
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
                    ExportEntity.DefaultAliased (name |> Identifier.create)
                | Choice2Of2 name -> ExportEntity.Named (name |> Identifier.create)
            )
            , ``module`` path
        )



[<RequireQualifiedAccess>]
module Comment =
    let create comment =
        Statement.Comment comment


[<RequireQualifiedAccess>]
module DTsTypes =
    let plainType =
        List.map Identifier.create >> DTsType.Plain
        
    let genericType qualifiers typeNames =
        (
            qualifiers |> List.map Identifier.create
            , typeNames
        )
        |> DTsType.Generic

    let voidType = DTsType.Void
    let undefinedType = DTsType.Undefined

    let funcSimple paramName paramPlainType returnPlainType =
        let fieldList : FieldList =
            (
                Field.Required (Identifier.create paramName),
                plainType paramPlainType |> TypeDefinition.Single
            )
            |> List.singleton

        (fieldList, plainType returnPlainType |> TypeDefinition.Single)
        |> DTsType.Func


[<RequireQualifiedAccess>]
module Structures =

    let typeAlias name typeCombination =
        (
            name |> Identifier.create
            , typeCombination
        ) |> TypeAlias.Plain |> StructureStatement.TypeAlias


let private typeName' typeName = 
    match typeName with
    | Choice1Of4 _ -> DTsType.Void
    | Choice2Of4 _ -> DTsType.Undefined
    | Choice3Of4 n -> DTsTypes.plainType n
    | Choice4Of4 _ -> DTsType.Any


[<RequireQualifiedAccess>]
module Fields =
    /// <summary>
    /// Example:
    /// <code> name: string; </code>
    /// </summary>
    /// <param name="name"></param>
    /// <param name="typeName"></param>
    let singleField name typeName =
        (
            Identifier.create name |> Required, 
            DTsTypes.plainType [typeName] |> TypeDefinition.Single
        )

    let singleArrayField name typeName =
        (
            Identifier.create name |> Required, 
            typeName' typeName |> DTsType.Array |> TypeDefinition.Single
        )


    /// <summary>
    /// Example:
    /// <code> name: string; </code>
    /// </summary>
    /// <param name="name"></param>
    /// <param name="typeName"></param>
    let single name typeName : FieldList = 
        singleField name typeName |> List.singleton


    /// <summary>
    /// Example:
    /// <code> id?: string | undefined; </code>
    /// </summary>
    /// <param name="name"></param>
    /// <param name="typeName"></param>
    let optionalUnionWithUndefinedField name typeName =
        (
            Identifier.create name |> Optional, 
            [
                DTsTypes.plainType typeName
                DTsType.Undefined
            ]
            |> Union |> TypeDefinition.Combination
        )

    /// <summary>
    /// Example:
    /// <code> id?: string | undefined; </code>
    /// </summary>
    /// <param name="name"></param>
    /// <param name="typeName"></param>
    let optionalUnionWithUndefined name typeName : FieldList =
        optionalUnionWithUndefinedField name typeName |> List.singleton



    let optionalFuncEmptyField name typeName : Field * TypeDefinition =
        (Identifier.create name, []) |> FuncOpt,
        typeName' typeName |> TypeDefinition.Single

    let requiredFuncEmptyField name typeName : Field * TypeDefinition =
        (Identifier.create name, []) |> FuncReq,
        typeName' typeName |> TypeDefinition.Single


    let optionalFuncField name parameter typeName : Field * TypeDefinition =
        (Identifier.create name, (parameter ||> single) ) |> FuncOpt,
        typeName' typeName |> TypeDefinition.Single


[<RequireQualifiedAccess>]
module Functions =
    /// <summary>
    /// Choices:
    ///
    /// 1. void
    /// 
    /// 2. undefined
    ///
    /// 3. plain type
    ///
    /// 4. any
    ///
    /// </summary>
    /// <param name="name"></param>
    /// <param name="parameters"></param>
    /// <param name="returnType"></param>
    let plain name parameters returnType =
        let i = Identifier.create name
        let parameters' : FieldList =
            parameters
            |> List.map (fun t ->
                let (i, tn) = t
                let paramName = Identifier.create i |> Field.Required
                let tn' = typeName' tn |> TypeDefinition.Single
                (paramName, tn')
            )
        let rtn = typeName' returnType |> TypeDefinition.Single
        (i, parameters', rtn) |> FunctionDefinition.Plain |> StructureStatement.FunctionDefinition