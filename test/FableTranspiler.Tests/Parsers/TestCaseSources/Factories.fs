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
        let generic = Dsl.Structures.genericTypeName ["React"; "HTMLProps"] [Dsl.Structures.plainTypeName ["HTMLButtonElement"]]
        let plain = Dsl.Structures.plainTypeName ["ReactScrollLinkProps"]
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
        let generic = Dsl.Structures.genericTypeName ["React"; "Component"] [Dsl.Structures.plainTypeName ["ButtonProps"]]
        let definition = 
            (Identifier.Create "Button", generic) 
            |> ClassDefinition.Extended
            |> ClassDefinition
        (input, definition)