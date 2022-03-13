namespace FableTranspiler.Tests.Parsers

open NUnit.Framework

[<Category("Parsers.StructuresTests")>]
module StructuresTests =

    open FsUnit
    open FableTranspiler.Parsers.Types
    open FableTranspiler.SimpleTypes
    open FableTranspiler.Tests.Common.FsUnit
    open FableTranspiler.Parsers
    open FParsec
    open FableTranspiler.Tests.Common
    open FsToolkit.ErrorHandling

    // -------------------
    //       Types        
    // -------------------


    [<TestCase("Foo")>]
    [<TestCase("Foo.Bar")>]
    let ``plane type test`` (input: string) =
        let planeType = Dsl.DTsTypes.plainType (input.Split('.') |> Array.toList)
        let result = run Structures.planeType input
        result |> shouldSuccessEqual planeType
        let result = run Structures.``type`` input
        result |> shouldSuccessEqual planeType

    [<TestCase("Foo", "Bar")>]
    [<TestCase("Foo.Bar", "Foo.Bar")>]
    let ``generic type test`` (input1: string, input2: string) =
        let planeType = Dsl.DTsTypes.plainType (input2.Split('.') |> Array.toList)
        let generic = Dsl.DTsTypes.genericType (input1.Split('.') |> Array.toList) [planeType]
        let input = (input1 + "<" + input2 + ">")
        let result = run Structures.genericType input
        result |> shouldSuccessEqual generic
        let result = run Structures.``type`` input
        result |> shouldSuccessEqual generic


    [<Test>]
    let ``type composition test`` () =
        let input = "ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>"
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let composition = [plain; generic] |> Composition
        let result = run Structures.typeComposition input
        result |> shouldSuccessEqual composition

    [<Test>]
    let ``type union test`` () =
        let input = "ReactScrollLinkProps | React.HTMLProps<HTMLButtonElement>"
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let composition = [plain; generic] |> Union
        let result = run Structures.typeUnion input
        result |> shouldSuccessEqual composition


    [<Test>]
    let ``type func test`` () =
        let input = "((distance: number) => number)"
        let field = Dsl.Fields.single "distance" "number"
        let type' = Dsl.DTsTypes.plainType ["number"] |> TypeDefinition.Single

        let expected = DTsType.Func (field, type')

        let result = run Structures.``type`` input
        result |> shouldSuccessEqual expected

    [<TestCaseSource(typeof<TestCases>, nameof(TestCases.DTsTypeCases))>]
    let ``type test`` (input: string, expected: DTsType) = 
        let result = run Structures.``type`` input
        result |> shouldSuccessEqual expected

    // -------------------
    //       Field        
    // -------------------

    [<TestCaseSource(typeof<TestCases>, nameof(TestCases.FieldCases))>]
    let ``field test`` (input: string, expected: (Field * TypeDefinition)) = 
        let result = run Structures.field input
        result |> shouldSuccessEqual expected


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
            (Identifier.create "duration" |> Optional, typeDefinitions)

        let result = run Structures.field input
        result |> shouldSuccessEqual expected


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
        result |> shouldSuccessEqual expected


    [<TestCase("type LinkProps = React.HTMLProps<HTMLButtonElement> & ReactScrollLinkProps;")>]
    let ``type alias of generic & plain test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([generic; plain] |> TypeCombination.Composition)

        let result = run Structures.statement input
        result |> shouldSuccessEqual expected


    [<TestCase("type LinkProps = ReactScrollLinkProps | React.HTMLProps<HTMLButtonElement>;")>]
    let ``type alias of plain | generic test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([plain; generic] |> TypeCombination.Union)

        let result = run Structures.statement input
        result |> shouldSuccessEqual expected


    [<TestCase("type LinkProps = React.HTMLProps<HTMLButtonElement> | ReactScrollLinkProps;")>]
    let ``type alias of generic | plain test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let expected = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([generic; plain] |> TypeCombination.Union)

        let result = run Structures.statement input
        result |> shouldSuccessEqual expected


    // -------------------
    //      Classes
    // -------------------

    [<TestCase("class Button extends React.Component<ButtonProps> {}")>]
    let ``class extends generic empty test`` (input: string) =
        let generic = Dsl.DTsTypes.genericType ["React"; "Component"] [Dsl.DTsTypes.plainType ["ButtonProps"]]
        let expected = 
            (Identifier.create "Button", generic) 
            |> ClassDefinition.ExtendsEmpty
            |> StructureStatement.ClassDefinition

        let result = run Structures.statement input
        result |> shouldSuccessEqual expected


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
            (Identifier.create "ElementProps", generic, oliteral) 
            |> InterfaceDefinition.Extends
            |> StructureStatement.InterfaceDefinition

        let result = run Structures.statement input
        result |> shouldSuccessEqual expected
    

    [<Test>]
    let ``interface plain not empty test`` () =
        let (input, definition) = StructuresFactory.interfaceDefinitioPlain

        let result = run Structures.statement input
        result |> shouldSuccessEqual definition


    // -------------------
    //     Functions
    // -------------------
    [<TestCaseSource(typeof<TestCases>, nameof(TestCases.FunctionCases))>]
    let ``function test`` (input: string, expected: StructureStatement) =
        let result = run Structures.statement input
        result |> shouldSuccessEqual expected


    // -------------------
    //   declare module
    // -------------------
    [<Test>]
    let ``declare module test`` () =
        result {
            let statement =
                """
                    declare module 'material-ui/svg-icons/av/mic-none' {
                    }
                """

            let! res = Parser.run statement
            ()
        }
        |> Result.runTest

    [<Test>]
    let ``declared module with exported imports test`` () =
        result {
            let statement =
                """
                declare module 'material-ui/svg-icons' {
                    // DO NOT EDIT
                    // This code is generated by scripts/material-ui/generate.js
                    // {{{
                        export import ActionAccessibility = __MaterialUI.SvgIcon; // require('material-ui/svg-icons/action/accessibility');
                        export import ActionAccessible = __MaterialUI.SvgIcon; // require('material-ui/svg-icons/action/accessible');
                        export import ActionAccountBalance = __MaterialUI.SvgIcon; // require('material-ui/svg-icons/action/account-balance');
                    // }}}
                        export import NavigationArrowDropRight = __MaterialUI.SvgIcon; // require('material-ui/svg-icons/navigation-arrow-drop-right');
                    }
                """

            let! res = Parser.run statement
            res |> should haveLength 1
            match res[0] with
            | Statement.ModuleDeclaration (_, md) ->
                md |> should haveLength 12
            | _ -> raise (AssertionException("first statement is not module declaration"))
        }
        |> Result.runTest