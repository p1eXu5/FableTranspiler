namespace FableTranspiler.Tests.Parsers

open NUnit.Framework
open FableTranspiler.Parsers.Types

[<Category("Parsers.StructuresTests")>]
module StructuresTests =

    open FableTranspiler.Parsers
    open FParsec
    open FableTranspiler.Tests.Parsers.Common


    [<TestCase("Foo")>]
    [<TestCase("Foo.Bar")>]
    let ``plane type test`` (input: string) =
        let planeType = Dsl.Structures.plainTypeName (input.Split('.') |> Array.toList)
        let result = run Structures.planeType input
        result |> shouldSuccess planeType
        let result = run Structures.``type`` input
        result |> shouldSuccess planeType

    [<TestCase("Foo", "Bar")>]
    [<TestCase("Foo.Bar", "Foo.Bar")>]
    let ``generic type test`` (input1: string, input2: string) =
        let planeType = Dsl.Structures.plainTypeName (input2.Split('.') |> Array.toList)
        let generic = Dsl.Structures.genericTypeName (input1.Split('.') |> Array.toList) [planeType]
        let input = (input1 + "<" + input2 + ">")
        let result = run Structures.genericType input
        result |> shouldSuccess generic
        let result = run Structures.``type`` input
        result |> shouldSuccess generic


    [<Test>]
    let ``type composition test`` () =
        let input = "ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>"
        let generic = Dsl.Structures.genericTypeName ["React"; "HTMLProps"] [Dsl.Structures.plainTypeName ["HTMLButtonElement"]]
        let plain = Dsl.Structures.plainTypeName ["ReactScrollLinkProps"]
        let composition = [plain; generic] |> Composition
        let result = run Structures.typeComposition input
        result |> shouldSuccess composition

    [<Test>]
    let ``type union test`` () =
        let input = "ReactScrollLinkProps | React.HTMLProps<HTMLButtonElement>"
        let generic = Dsl.Structures.genericTypeName ["React"; "HTMLProps"] [Dsl.Structures.plainTypeName ["HTMLButtonElement"]]
        let plain = Dsl.Structures.plainTypeName ["ReactScrollLinkProps"]
        let composition = [plain; generic] |> Union
        let result = run Structures.typeUnion input
        result |> shouldSuccess composition



    [<TestCase("type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;")>]
    let ``type alias of plain & generic test`` (input: string) =
        let generic = Dsl.Structures.genericTypeName ["React"; "HTMLProps"] [Dsl.Structures.plainTypeName ["HTMLButtonElement"]]
        let plain = Dsl.Structures.plainTypeName ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([plain; generic] |> TypeCombination.Composition)
            |> Structure

        let result = run Structures.statement input
        result |> shouldSuccess expected


    [<TestCase("type LinkProps = React.HTMLProps<HTMLButtonElement> & ReactScrollLinkProps;")>]
    let ``type alias of generic & plain test`` (input: string) =
        let result = run Structures.statement input
        let generic = Dsl.Structures.genericTypeName ["React"; "HTMLProps"] [Dsl.Structures.plainTypeName ["HTMLButtonElement"]]
        let plain = Dsl.Structures.plainTypeName ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([generic; plain] |> TypeCombination.Composition)
            |> Structure

        result |> shouldSuccess expected


    [<TestCase("type LinkProps = ReactScrollLinkProps | React.HTMLProps<HTMLButtonElement>;")>]
    let ``type alias of plain | generic test`` (input: string) =
        let result = run Structures.statement input
        let generic = Dsl.Structures.genericTypeName ["React"; "HTMLProps"] [Dsl.Structures.plainTypeName ["HTMLButtonElement"]]
        let plain = Dsl.Structures.plainTypeName ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([plain; generic] |> TypeCombination.Union)
            |> Structure

        result |> shouldSuccess expected

    [<TestCase("type LinkProps = React.HTMLProps<HTMLButtonElement> | ReactScrollLinkProps;")>]
    let ``type alias of generic | plain test`` (input: string) =
        let result = run Structures.statement input
        let generic = Dsl.Structures.genericTypeName ["React"; "HTMLProps"] [Dsl.Structures.plainTypeName ["HTMLButtonElement"]]
        let plain = Dsl.Structures.plainTypeName ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([generic; plain] |> TypeCombination.Union)
            |> Structure

        result |> shouldSuccess expected


    // -------------------

    [<TestCase("class Button extends React.Component<ButtonProps> {}")>]
    let ``class extended generic test`` (input: string) =
        let generic = Dsl.Structures.genericTypeName ["React"; "Component"] [Dsl.Structures.plainTypeName ["ButtonProps"]]
        let expected = 
            (Identifier.Create "Button", generic) 
            |> ClassDefinition.Extended
            |> ClassDefinition
            |> Structure

        let result = run Structures.statement input
        result |> shouldSuccess expected



    