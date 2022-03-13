namespace FableTranspiler.Tests.Domain.Interpreters.FsInterpreter

open System.Collections
open NUnit.Framework
open FParsec
open FableTranspiler.Parsers
open FableTranspiler.Parsers.Types
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Interpreters
open System

[<AutoOpen>]
module private Helpers =

    let parse parser input =
        run parser input
        |> function
            | Success (ok,_,_) -> ok
            | Failure (err,_,_) -> failwith err

    let replaceDot (s: string) = s.Replace(".", "\u02CC")

type ReactTestCases () =

    static member FieldTests : IEnumerable =
        [
            "to: string;",                                                              "| To of string"
            "to?: string;",                                                             "| To of string"
            "spy?: boolean | undefined;",                                               "| Spy of bool"
            "smooth?: boolean | string | undefined;",                                   "| Smooth of U2<bool, string>"
            "onClick?(): void;",                                                        "| OnClick of unit -> unit"
            "onSetActive?(to: string): void;",                                          "| OnSetActive of string -> unit"
            "duration?: number | string | ((distance: number) => number) | undefined;", "| Duration of U3<float, string, (float -> float)>"
        ]
        |> Seq.map (fun (dts, fs) ->
            TestCaseData(exportedInterfaceWith dts |> box, fs).SetName(replaceDot $"Field: '{dts}'")
        )
        :> _

    static member DtsTypeCases : IEnumerable =
        [
            "React.ComponentType<TProps>", "ReactElementType<'TProps>", []
            "React.ComponentClass<ScrollElementProps<P>>", "ReactElement", []
        ]
        |> Seq.map (fun (dts, fs, summ) ->
            TestCaseData(Helpers.parse Structures.``type`` dts, fs, summ).SetName(replaceDot $"DTsType: '{dts}'")
        )
        :> _