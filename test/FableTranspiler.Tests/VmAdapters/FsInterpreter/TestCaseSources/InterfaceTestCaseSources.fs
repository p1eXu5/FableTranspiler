module FableTranspiler.Tests.VmAdapters.TestCaseSources.InterfaceTestCaseSources

open System.Collections
open NUnit.Framework

let private exportedInterfaceFormatted =
    """
        export interface Foo {
            {field}
        }
    """

let exportedInterface field =
    exportedInterfaceFormatted.Replace("{field}", field)


type TestCases () =

    static member ExportCases : IEnumerable =
        seq {

            TestCaseData(
                exportedInterface "to: string;" |> box,
                "abstract to : string"
            ).SetName("Field: 'to: string;'")

            TestCaseData(
                exportedInterface "smooth?: boolean | string | undefined;" |> box,
                "abstract smooth : U2<bool, string> option"
            ).SetName("Field: 'smooth?: boolean | string | undefined;'")

            TestCaseData(
                exportedInterface "onClick?(): void;" |> box,
                "abstract onClick : (unit -> unit) option"
            ).SetName("Field: 'onClick?(): void;'")

            TestCaseData(
                exportedInterface "onSetActive?(to: string): void;" |> box,
                "abstract onSetActive : (string -> unit) option"
            ).SetName("Field: 'onSetActive?(to: string): void;'")

            TestCaseData(
                exportedInterface "duration?: number | string | ((distance: number) => number) | undefined;" |> box,
                "abstract duration : U3<float, string, (float -> float)> option"
            ).SetName("Field: 'duration?: number | string | ((distance: number) => number) | undefined;'")
        }