namespace FableTranspiler.Tests.ParsersTests

open System
open System.Collections
open NUnit.Framework
open FableTranspiler.Parsers.Types
open FableTranspiler.Parsers.Dsl

type TestCases () =
    static member ImportCases : Object[] =
        [|
            [|
                """import * as React from 'react';""" |> box
                Import.allAliased "React" "react"
            |]

            [|
                """import * as scroller from './mixins/scroller';""" |> box
                Import.allAliased "scroller" "./mixins/scroller"
            |]

            [|
                """import { ReactScrollLinkProps } from './Link';""" |> box
                Import.namedS "ReactScrollLinkProps" "./mixins/scroller"
            |]
        |]


            //yield TestCaseData("""import { ZipCodeValidator as ZCV } from "./ZipCodeValidator";""")
            //    .Returns(
            //        Statement.Import (
            //            [ImportEntity.Aliased ("ZipCodeValidator" |> Identifier.Create, "ZCV" |> Identifier.Create)]
            //            , ModulePath.Relative (System.Uri("./ZipCodeValidator", UriKind.Relative))
            //        ) |> Ok)


            //yield TestCaseData("""import "./my-module.js";""")
            //    .Returns(
            //        Statement.Import (
            //            [ImportEntity.No]
            //            , ModulePath.Relative (System.Uri("./my-module.js", UriKind.Relative))
            //        ) |> Ok)


            //yield TestCaseData("""import $ from "jquery";""")
            //    .Returns(
            //        Statement.Import (
            //            [ImportEntity.Named ("$" |> Identifier.Create)]
            //            , ModulePath.NodeModule (System.Uri("jquery", UriKind.Relative))
            //        ) |> Ok)

        //}