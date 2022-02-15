namespace FableTranspiler.Tests

module HelpersTests =

    open NUnit.Framework
    open FsUnit
    open FsToolkit.ErrorHandling
    open FableTranspiler.SimpleTypes
    open FableTranspiler.Helpers

    [<TestCase("D:\\foo", "./components/Link", 2)>]
    let ``nesting tests`` (libLocationValue, modulePathValue, expectedNesting) =
        result {
            let! libLocation = LibLocation.Create libLocationValue
            let! modulePath = ModulePath.Create modulePathValue
            return nesting libLocation modulePath
        }
        |> should be (ofCase <@ Ok expectedNesting @>)