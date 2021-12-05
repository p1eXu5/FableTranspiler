namespace FableTranspiler.Tests.ParsersTests

open System
open System.Collections
open NUnit.Framework
open FableTranspiler.Parsers.Types

type TestCases () =
    static member ImportCases : Object[] =
        [|
            [|
                """import * as React from 'react';""" |> box
                (
                    Statement.Import (
                        [ImportEntity.AllAliased ("React" |> Identifier.Create)]
                        , ImportModule.NodeModule (ModulePath.Create "react")
                    )
                )
            |]
        |]


            //yield TestCaseData("""import * as scroller from './mixins/scroller';""")
            //    .Returns(
            //        Statement.Import (
            //            [ImportEntity.AllAliased ("scroller" |> Identifier.Create)]
            //            , ModulePath.Relative (System.Uri("./mixins/scroller", UriKind.Relative))
            //        ) |> Ok)


            //yield TestCaseData("""import { ReactScrollLinkProps } from './Link';""")
            //    .Returns(
            //        Statement.Import (
            //            [ImportEntity.Named ("ReactScrollLinkProps" |> Identifier.Create)]
            //            , ModulePath.Relative (System.Uri("./Link", UriKind.Relative))
            //        ) |> Ok)


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