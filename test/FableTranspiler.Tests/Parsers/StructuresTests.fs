namespace FableTranspiler.Tests.Parsers

open NUnit.Framework

[<Category("Parsers.StructuresTests")>]
module StructuresTests =

    open FableTranspiler.Parsers
    open FParsec
    open FableTranspiler.Tests.Parsers.Common


    [<TestCase("export type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;")>]
    [<TestCase("type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;")>]
    let ``export alias type`` (input: string) =
        let result = run Structures.statement input
        let generic = Dsl.Structures.genericType ["React"; "HTMLProps"]
        let plain = Dsl.Structures.plainType ["ReactScrollLinkProps"]
        let expected = Dsl.Structures.typeAlias "LinkProps" [plain; generic]
        result |> shouldSuccess expected
