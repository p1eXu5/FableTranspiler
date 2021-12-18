namespace FableTranspiler.Tests.Parsers

open System
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
                    Import.allAliased "React" "react").SetName("Statement: allAliased 1")
            yield 
                TestCaseData(
                    """import * as scroller from './mixins/scroller';""" |> box,
                    Import.allAliased "scroller" "./mixins/scroller").SetName("Statement: allAliased 2")
            yield 
                TestCaseData(
                    """import { ReactScrollLinkProps } from './Link';""" |> box,
                    Import.namedS "ReactScrollLinkProps" "./Link").SetName("Statement: named")
            yield 
                TestCaseData(
                    """import { ZipCodeValidator as ZCV } from './ZipCodeValidator';""" |> box,
                    Import.aliasedS "ZipCodeValidator" "ZCV" "./ZipCodeValidator").SetName("Statement: aliased")
            yield 
                TestCaseData(
                    """import './my-module.js';""" |> box,
                    Import.``default`` "./my-module.js").SetName("Statement: no")
        }
