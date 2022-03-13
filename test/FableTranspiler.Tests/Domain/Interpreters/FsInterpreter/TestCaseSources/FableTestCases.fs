namespace FableTranspiler.Tests.Domain.Interpreters.FsInterpreter

open System.Collections
open NUnit.Framework

type FableTestCases () =

    static member FieldTests : IEnumerable =
        seq {
            TestCaseData(
                exportedInterfaceWith "to: string;" |> box,
                "abstract to : string"
            ).SetName("Field: 'to: string;'")

            TestCaseData(
                exportedInterfaceWith "spy?: boolean | undefined;" |> box,
                "abstract spy : bool option"
            ).SetName("Field: 'spy?: boolean | undefined;'")

            TestCaseData(
                exportedInterfaceWith "smooth?: boolean | string | undefined;" |> box,
                "abstract smooth : U2<bool, string> option"
            ).SetName("Field: 'smooth?: boolean | string | undefined;'")

            TestCaseData(
                exportedInterfaceWith "onClick?(): void;" |> box,
                "abstract onClick : (unit -> unit) option"
            ).SetName("Field: 'onClick?(): void;'")

            TestCaseData(
                exportedInterfaceWith "onSetActive?(to: string): void;" |> box,
                "abstract onSetActive : (string -> unit) option"
            ).SetName("Field: 'onSetActive?(to: string): void;'")

            TestCaseData(
                exportedInterfaceWith "duration?: number | string | ((distance: number) => number) | undefined;" |> box,
                "abstract duration : U3<float, string, (float -> float)> option"
            ).SetName("Field: 'duration?: number | string | ((distance: number) => number) | undefined;'")
        }