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
        }