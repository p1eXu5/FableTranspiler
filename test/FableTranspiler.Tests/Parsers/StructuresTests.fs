namespace FableTranspiler.Tests.Parsers

open NUnit.Framework
open FableTranspiler.Parsers.Types
open FableTranspiler.SimpleTypes

[<Category("Parsers.StructuresTests")>]
module StructuresTests =

    open FableTranspiler.Parsers
    open FParsec
    open FableTranspiler.Tests.Common

    // -------------------
    //       Types        
    // -------------------


    [<TestCase("Foo")>]
    [<TestCase("Foo.Bar")>]
    let ``plane type test`` (input: string) =
        let planeType = Dsl.DTsTypes.plainType (input.Split('.') |> Array.toList)
        let result = run Structures.planeType input
        result |> shouldSuccess planeType
        let result = run Structures.``type`` input
        result |> shouldSuccess planeType

    [<TestCase("Foo", "Bar")>]
    [<TestCase("Foo.Bar", "Foo.Bar")>]
    let ``generic type test`` (input1: string, input2: string) =
        let planeType = Dsl.DTsTypes.plainType (input2.Split('.') |> Array.toList)
        let generic = Dsl.DTsTypes.genericType (input1.Split('.') |> Array.toList) [planeType]
        let input = (input1 + "<" + input2 + ">")
        let result = run Structures.genericType input
        result |> shouldSuccess generic
        let result = run Structures.``type`` input
        result |> shouldSuccess generic


    [<Test>]
    let ``type composition test`` () =
        let input = "ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>"
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let composition = [plain; generic] |> Composition
        let result = run Structures.typeComposition input
        result |> shouldSuccess composition

    [<Test>]
    let ``type union test`` () =
        let input = "ReactScrollLinkProps | React.HTMLProps<HTMLButtonElement>"
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let composition = [plain; generic] |> Union
        let result = run Structures.typeUnion input
        result |> shouldSuccess composition


    [<Test>]
    let ``type func test`` () =
        let input = "((distance: number) => number)"
        let field = Dsl.Fields.single "distance" "number"
        let type' = Dsl.DTsTypes.plainType ["number"] |> TypeDefinition.Single

        let expected = DTsType.Func (field, type')

        let result = run Structures.``type`` input
        result |> shouldSuccess expected

    [<TestCaseSource(typeof<TestCases>, nameof(TestCases.DTsTypeCases))>]
    let ``type test`` (input: string, expected: DTsType) = 
        let result = run Structures.``type`` input
        result |> shouldSuccess expected

    // -------------------
    //       Field        
    // -------------------

    [<TestCaseSource(typeof<TestCases>, nameof(TestCases.FieldCases))>]
    let ``field test`` (input: string, expected: (Field * TypeDefinition)) = 
        let result = run Structures.field input
        result |> shouldSuccess expected


    [<Test>]
    let ``field of union with func test`` () =
        let input = "duration?: number | string | ((distance: number) => number) | undefined;"
        let typeDefinitions =
            TypeCombination.Union [
                Dsl.DTsTypes.plainType ["number"]
                Dsl.DTsTypes.plainType ["string"]
                Dsl.DTsTypes.funcSimple "distance" ["number"] ["number"]
                Dsl.DTsTypes.undefinedType
            ]
            |> TypeDefinition.Combination
        let expected =
            (Identifier.Create "duration" |> Optional, typeDefinitions)

        let result = run Structures.field input
        result |> shouldSuccess expected


    // -------------------
    //      TypeAlias
    // -------------------

    [<TestCase("type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;")>]
    let ``type alias of plain & generic test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([plain; generic] |> TypeCombination.Composition)

        let result = run Structures.statement input
        result |> shouldSuccess expected


    [<TestCase("type LinkProps = React.HTMLProps<HTMLButtonElement> & ReactScrollLinkProps;")>]
    let ``type alias of generic & plain test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([generic; plain] |> TypeCombination.Composition)

        let result = run Structures.statement input
        result |> shouldSuccess expected


    [<TestCase("type LinkProps = ReactScrollLinkProps | React.HTMLProps<HTMLButtonElement>;")>]
    let ``type alias of plain | generic test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([plain; generic] |> TypeCombination.Union)

        let result = run Structures.statement input
        result |> shouldSuccess expected


    [<TestCase("type LinkProps = React.HTMLProps<HTMLButtonElement> | ReactScrollLinkProps;")>]
    let ``type alias of generic | plain test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([generic; plain] |> TypeCombination.Union)

        let result = run Structures.statement input
        result |> shouldSuccess expected


    // -------------------
    //      Classes
    // -------------------

    [<TestCase("class Button extends React.Component<ButtonProps> {}")>]
    let ``class extends generic empty test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "Component"] [Dsl.DTsTypes.plainType ["ButtonProps"]]
        let expected = 
            (Identifier.Create "Button", generic) 
            |> ClassDefinition.ExtendsEmpty
            |> StructureStatement.ClassDefinition

        let result = run Structures.statement input
        result |> shouldSuccess expected


    // -------------------
    //     Interfaces
    // -------------------

    [<TestCase("""interface ElementProps extends React.HTMLProps<HTMLDivElement> {
    name: string;
    id?: string | undefined;
    spyCallbacks: any[];
}""")>]
    let ``interface extends generic not empty test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLDivElement"]]
        let field1 = Dsl.Fields.singleField "name" "string"
        let field2 = Dsl.Fields.optionalUnionWithUndefinedField "id" ["string"]
        let field3 = Dsl.Fields.singleArrayField "spyCallbacks" (Choice4Of4 ())

        let oliteral : FieldList = [field1; field2; field3;]

        let expected = 
            (Identifier.Create "ElementProps", generic, oliteral) 
            |> InterfaceDefinition.Extends
            |> StructureStatement.InterfaceDefinition

        let result = run Structures.statement input
        result |> shouldSuccess expected
    

    [<Test>]
    let ``interface plain not empty test`` () =
        let (input, definition) = StructuresFactory.interfaceDefinitioPlain

        let result = run Structures.statement input
        result |> shouldSuccess definition


    // -------------------
    //     Functions
    // -------------------
    [<TestCaseSource(typeof<TestCases>, nameof(TestCases.FunctionCases))>]
    let ``function test`` (input: string, expected: StructureStatement) =
        let result = run Structures.statement input
        result |> shouldSuccess expected