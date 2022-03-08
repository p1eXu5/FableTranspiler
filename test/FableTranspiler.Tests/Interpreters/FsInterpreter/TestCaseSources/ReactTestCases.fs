namespace FableTranspiler.Tests.Domain.Interpreters.FsInterpreter

open System.Collections
open NUnit.Framework

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
            TestCaseData(exportedInterfaceWith dts |> box, fs).SetName($"Field: '{dts}'")
        )
        :> _
