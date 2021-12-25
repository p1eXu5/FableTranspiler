[<AutoOpen>]
module FableTranspiler.Tests.Parsers.Factories

open FableTranspiler.Parsers
open FableTranspiler.Parsers.Types

[<RequireQualifiedAccess>]
module StructuresFactory =

    /// <summary>
    /// <c>"type LinkProps = ReactScrollLinkProps &amp; React.HTMLProps&lt;HTMLButtonElement&gt;;"</c>
    /// </summary>
    let typeAliasComposition =
        let input = "type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;"
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLButtonElement"]]
        let plain = Dsl.DTsTypes.plainType ["ReactScrollLinkProps"]
        let composition = 
            Dsl.Structures.typeAlias 
                "LinkProps" 
                ([plain; generic] |> TypeCombination.Composition)
        (input, composition)


    /// <summary>
    /// <c>"class Button extends React.Component&lt;ButtonProps&gt; {}"</c>
    /// </summary>
    let classDefinition =
        let input = "class Button extends React.Component<ButtonProps> {}"
        let generic = Dsl.DTsTypes.genericType ["React"; "Component"] [Dsl.DTsTypes.plainType ["ButtonProps"]]
        let definition = 
            (Identifier.Create "Button", generic) 
            |> ClassDefinition.ExtendsEmpty
            |> StructureStatement.ClassDefinition
        (input, definition)


    /// <summary>
    /// <code>
    /// interface ElementProps extends React.HTMLProps&lt;HTMLDivElement&gt; {
    ///     name: string;
    ///     id?: string | undefined;
    /// }
    /// </code>
    /// </summary>
    let interfaceDefinitioExtends =
        let input = """interface ElementProps extends React.HTMLProps<HTMLDivElement> {
    name: string;
    id?: string | undefined;
}"""
        let generic = Dsl.DTsTypes.genericType ["React"; "HTMLProps"] [Dsl.DTsTypes.plainType ["HTMLDivElement"]]
        let field1 = Dsl.Fields.singleField "name" "string"
        let field2 = Dsl.Fields.optionalUnionWithUndefinedField "id" ["string"]

        let oliteral : FieldList = [field1; field2]

        let definition = 
            (Identifier.Create "ElementProps", generic, oliteral) 
            |> InterfaceDefinition.Extends
            |> StructureStatement.InterfaceDefinition
        (input, definition)


    /// <summary>
    /// <code>
    /// interface ElementProps {
    ///     name: string;
    ///     id?: string | undefined;
    /// }
    /// </code>
    /// </summary>
    let interfaceDefinitioPlain =
        let input = """interface ElementProps {
    name: string;
    id?: string | undefined;
}"""
        let field1 = Dsl.Fields.singleField "name" "string"
        let field2 = Dsl.Fields.optionalUnionWithUndefinedField "id" ["string"]

        let oliteral : FieldList = [field1; field2]

        let definition = 
            (Identifier.Create "ElementProps", oliteral) 
            |> InterfaceDefinition.Plain
            |> StructureStatement.InterfaceDefinition
        (input, definition)

