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


[<RequireQualifiedAccess>]
module TypeNames =
    let plainTypeName =
        List.map Identifier.Create >> TypeName.Plain
        
    let genericTypeName qualifiers typeNames =
        (
            qualifiers |> List.map Identifier.Create
            , typeNames
        )
        |> TypeName.Generic

    let voidType = TypeName.Void
    let undefinedType = TypeName.Undefined

    let funcSimple paramName paramPlainType returnPlainType =
        let fieldList : FieldList =
            (
                Field.Required (Identifier.Create paramName),
                plainTypeName paramPlainType |> TypeDefinition.Single
            )
            |> List.singleton

        (fieldList, plainTypeName returnPlainType |> TypeDefinition.Single)
        |> TypeName.Func


[<RequireQualifiedAccess>]
module Structures =

    let typeAlias name typeCombination =
        (
            name |> Identifier.Create
            , typeCombination
        ) |> TypeAlias


let private typeName' typeName = 
    match typeName with
    | Choice1Of4 _ -> TypeName.Void
    | Choice2Of4 _ -> TypeName.Undefined
    | Choice3Of4 n -> TypeNames.plainTypeName n
    | Choice4Of4 _ -> TypeName.Any


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
            Identifier.Create name |> Required, 
            TypeNames.plainTypeName [typeName] |> TypeDefinition.Single
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
            Identifier.Create name |> Optional, 
            [
                TypeNames.plainTypeName typeName
                TypeName.Undefined
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
        (Identifier.Create name, []) |> FuncOpt,
        typeName' typeName |> TypeDefinition.Single

    let requiredFuncEmptyField name typeName : Field * TypeDefinition =
        (Identifier.Create name, []) |> FuncReq,
        typeName' typeName |> TypeDefinition.Single


    let optionalFuncField name parameter typeName : Field * TypeDefinition =
        (Identifier.Create name, (parameter ||> single) ) |> FuncOpt,
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
    let create name parameters returnType =
        let i = Identifier.Create name
        let parameters' : FieldList =
            parameters
            |> List.map (fun t ->
                let (i, tn) = t
                let paramName = Identifier.Create i |> Field.Required
                let tn' = typeName' tn |> TypeDefinition.Single
                (paramName, tn')
            )
        let rtn = typeName' returnType |> TypeDefinition.Single
        (i, parameters', rtn) |> StructureStatement.FunctionDefinition