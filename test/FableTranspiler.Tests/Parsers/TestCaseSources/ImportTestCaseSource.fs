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
                    """export { default as Button } from './components/Button';""" |> box,
                    Dsl.Export.defaultAliasedS "Button" "./components/Button").SetName("Statement: export defaultAliased 1")
            yield 
                TestCaseData(
                    """export { Helpers } from './mixins/Helpers';""" |> box,
                    Dsl.Export.namedS "Helpers" "./mixins/Helpers").SetName("Statement: export named")

            yield 
                TestCaseData(
                    """export { animateScroll, scroller };""" |> box,
                    Dsl.Export.outList ["animateScroll"; "scroller"]).SetName("Statement: export outList")
            yield 
                TestCaseData(
                    """export = ReactScroll;""" |> box,
                    Dsl.Export.outAssignment "ReactScroll").SetName("Statement: export outAssignment")

            let (typeAliasInput, typeAlias) = StructuresFactory.typeAliasComposition
            yield
                TestCaseData(
                    $"""export {typeAliasInput}""" |> box,
                    typeAlias |> ExportStatement.Structure |> Statement.Export).SetName("Statement: export type alias")

            let (classDefinitionInput, classDefinition) = StructuresFactory.classDefinition
            yield
                TestCaseData(
                    $"""export default {classDefinitionInput}""" |> box,
                    classDefinition |> ExportStatement.Structure |> Statement.Export).SetName("Statement: export default class definition")
        }


    static member FieldCases : IEnumerable =
        seq {
            yield 
                TestCaseData(
                    """name: string;""" |> box,
                    Dsl.Literals.singleField "name" "string").SetName("Field: single type field")

            yield 
                TestCaseData(
                    """id?: string | undefined;""" |> box,
                    Dsl.Literals.optionalUnionWithUndefinedField "id" ["string"] ).SetName("Field: union with undefined")
        }
