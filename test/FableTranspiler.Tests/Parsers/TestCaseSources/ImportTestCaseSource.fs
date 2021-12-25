namespace FableTranspiler.Tests.Parsers

open System
open FableTranspiler.Parsers
open FableTranspiler.Parsers.Dsl
open System.Collections
open NUnit.Framework
open FParsec
open FParsec.Primitives
open FableTranspiler.Parsers.Types

type TestCases () =
    static member ImportCases : IEnumerable =
        seq {
            yield 
                TestCaseData(
                    """import * as React from 'react';""" |> box,
                    Dsl.Import.allAliased "React" "react").SetName("Statement: allAliased 1")
            yield 
                TestCaseData(
                    """import * as scroller from './mixins/scroller';""" |> box,
                    Dsl.Import.allAliased "scroller" "./mixins/scroller").SetName("Statement: allAliased 2")
            yield 
                TestCaseData(
                    """import { ReactScrollLinkProps } from './Link';""" |> box,
                    Dsl.Import.namedS "ReactScrollLinkProps" "./Link").SetName("Statement: named")
            yield 
                TestCaseData(
                    """import { ZipCodeValidator as ZCV } from './ZipCodeValidator';""" |> box,
                    Dsl.Import.aliasedS "ZipCodeValidator" "ZCV" "./ZipCodeValidator").SetName("Statement: aliased")
            yield 
                TestCaseData(
                    """import './my-module.js';""" |> box,
                    Dsl.Import.``default`` "./my-module.js").SetName("Statement: no")
        }


    static member ExportCases : IEnumerable =
        seq {
            yield 
                TestCaseData(
                    "export { default as Button } from './components/Button';" |> box,
                    Dsl.Export.defaultAliasedS "Button" "./components/Button").SetName("Statement: export defaultAliased 1")
            yield 
                TestCaseData(
                    "export { Helpers } from './mixins/Helpers';" |> box,
                    Dsl.Export.namedS "Helpers" "./mixins/Helpers").SetName("Statement: export named")

            yield 
                TestCaseData(
                    "export { animateScroll, scroller };" |> box,
                    Dsl.Export.outList ["animateScroll"; "scroller"]).SetName("Statement: export outList")
            yield 
                TestCaseData(
                    "export = ReactScroll;" |> box,
                    Dsl.Export.outAssignment "ReactScroll").SetName("Statement: export outAssignment")

            let (typeAliasInput, typeAlias) = StructuresFactory.typeAliasComposition
            yield
                TestCaseData(
                    $"export {typeAliasInput}" |> box,
                    typeAlias |> ExportStatement.Structure |> Statement.Export).SetName("Statement: export type alias")

            let (classDefinitionInput, classDefinition) = StructuresFactory.classDefinition
            yield
                TestCaseData(
                    $"export default {classDefinitionInput}" |> box,
                    classDefinition |> ExportStatement.StructureDefault |> Statement.Export).SetName("Statement: export default class definition")

            let (interfaceDefinitionInput, interfaceDefinition) = StructuresFactory.interfaceDefinitioExtends
            yield
                TestCaseData(
                    $"export {interfaceDefinitionInput}" |> box,
                    interfaceDefinition |> ExportStatement.Structure |> Statement.Export).SetName("Statement: export interface definition")

            let (interfaceDefinitionPlainInput, interfaceDefinitionPlain) = StructuresFactory.interfaceDefinitioPlain
            yield
                TestCaseData(
                    $"export {interfaceDefinitionPlainInput}" |> box,
                    interfaceDefinitionPlain |> ExportStatement.Structure |> Statement.Export).SetName("Statement: export interface definition plain")
        }


    static member FieldCases : IEnumerable =
        seq {
            yield 
                TestCaseData(
                    "name: string;" |> box,
                    Dsl.Fields.singleField "name" "string").SetName("Field: single type field")

            yield 
                TestCaseData(
                    "id?: string | undefined;" |> box,
                    Dsl.Fields.optionalUnionWithUndefinedField "id" ["string"] ).SetName("Field: union with undefined")

            yield 
                TestCaseData(
                    "onClick?(): void;" |> box,
                    Dsl.Fields.optionalFuncEmptyField "onClick" (Choice1Of4 ()) ).SetName("Field: func?(): void")

            yield 
                TestCaseData(
                    "onClick(): void;" |> box,
                    Dsl.Fields.requiredFuncEmptyField "onClick" (Choice1Of4 ()) ).SetName("Field: func(): void")

            yield 
                TestCaseData(
                    "onSetActive?(to: string): void;" |> box,
                    Dsl.Fields.optionalFuncField "onSetActive" ("to", "string") (Choice1Of4 ()) ).SetName("Field: func?(foo: bar): void")

            yield 
                TestCaseData(
                    "name: string[];" |> box,
                    Dsl.Fields.singleArrayField "name" (Choice3Of4 ["string"])).SetName("Field: single array type field")

            yield 
                TestCaseData(
                    "name: any[];" |> box,
                    Dsl.Fields.singleArrayField "name" (Choice4Of4 ())).SetName("Field: single any array type field")


            let field : FieldList = Dsl.Fields.singleField "x" "number"  |> List.singleton
            let typeDef = Dsl.DTsTypes.plainType ["number"] |> TypeDefinition.Single
            let expected = Field.Required (Identifier.Create "name"), DTsType.Func (field, typeDef) |> TypeDefinition.Single

            yield 
                TestCaseData(
                    "name: (x: number) => number;" |> box,
                    expected).SetName("Field: name: (x: number) => number;")

        }


    static member FunctionCases : IEnumerable =
        seq {
            yield 
                TestCaseData(
                    "function unmount(): void;" |> box,
                    Dsl.Functions.create "unmount" [] (Choice1Of4 ())).SetName("Functions: function unmount(): void;")


            let union = 
                [
                    Dsl.DTsTypes.plainType ["boolean"]
                    Dsl.DTsTypes.plainType ["string"]
                ]
                |> TypeCombination.Union
                |> TypeDefinition.Combination

            let objectLiteral : FieldList = (Field.Required (Identifier.Create "smooth"), union) |> List.singleton
            let field : FieldList = 
                (Identifier.Create "options" |> Field.Required, objectLiteral |> TypeDefinition.InlineObject) |> List.singleton


            let retFl : FieldList = Dsl.Fields.singleField "x" "number"  |> List.singleton
            let ret =
                (retFl, Dsl.DTsTypes.plainType ["number"] |> TypeDefinition.Single) |> DTsType.Func |> TypeDefinition.Single

            let expected = FunctionDefinition (Identifier.Create "getAnimationType", field, ret)

            yield 
                TestCaseData(
                    "function getAnimationType(options: { smooth: boolean | string }): (x: number) => number;" |> box,
                    expected).SetName("Functions: function getAnimationType(options: { smooth: boolean | string }): (x: number) => number;")
        }
