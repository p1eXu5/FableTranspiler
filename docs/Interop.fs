(*

    React.createElement(
        type,
        [props],
        [...children]
    )

    Create and return a new React element of the given type. 
    The type argument can be either a tag name string (such as 'div' or 'span'), 
    a React component type (a class or a function), or a React fragment type.
*)



// ********************
//        Fable
//        =====
// ********************

// =======================
// Fable.React.Standard.fs
// =======================

open Fable.React

let inline a props children = domEl "a" props children



// ======================
// Fable.React.Helpers.fs
// ======================

/// Instantiate a DOM React element
    let inline domEl (tag: string) (props: IHTMLProp seq) (children: ReactElement seq): ReactElement =
#if FABLE_COMPILER
        ReactBindings.React.createElement(tag, keyValueList CaseRules.LowerFirst props, children)
#else
        ServerRendering.createServerElement(tag, (props |> Seq.cast<IProp>), children, ServerElementType.Tag)
#endif


// ==============
// Fable.React.fs
// ==============

type IReactExports =
    abstract createElement: comp: obj * props: obj * [<ParamList>] children: ReactElement seq -> ReactElement
    abstract createContext: defaultValue: 'T -> IContext<'T>
    abstract createRef: initialValue: 'T -> IRefValue<'T>
    abstract forwardRef: fn: ('props -> IRefValue<'T> option -> ReactElement) -> ReactElementType<'props>
    abstract memo: render: ('props -> ReactElement) * areEqual: ('props -> 'props -> bool) -> ReactElementType<'props>
    abstract Fragment: ReactElementType<obj>
    abstract Suspense: ReactElementType<obj>
    abstract ``lazy``: f: (unit -> JS.Promise<'TIn>) -> 'TOut

module ReactBindings =
    #if FABLE_REPL_LIB
    [<Global("React")>]
    #else
    [<Import("*", "react")>]
    #endif
    /// Mainly intended for internal use
    let React: IReactExports = jsNative



// ********************
//        Feliz
//        =====
// ********************

// Client lib

[<Erase>]
type Mui =
    static member inline box props = Interop.createElement (importDefault "@material-ui/core/Box") props
    static member inline box (children: #seq<ReactElement>) = 
        Interop.createElement (importDefault "@material-ui/core/Box") [ p1eXu5.Fable.MaterialUI.box.children (children :> ReactElement seq) ]


// Interop.fs

[<RequireQualifiedAccess>]
module Interop =
    let reactApi : IReactApi = importDefault "react"
    #if FABLE_COMPILER_3
    let inline reactElement (name: string) (props: 'a) : ReactElement = import "createElement" "react"
    #else
    let reactElement (name: string) (props: 'a) : ReactElement = import "createElement" "react"
    #endif
    let inline mkAttr (key: string) (value: obj) : IReactProperty = unbox (key, value)
    [<Emit "undefined">]
    let undefined : obj = jsNative
    let inline mkStyle (key: string) (value: obj) : IStyleAttribute = unbox (key, value)
    let inline svgAttribute (key: string) (value: obj) : ISvgAttribute = unbox (key, value)
    let inline reactElementWithChild (name: string) (child: 'a) =
        reactElement name (createObj [ "children" ==> ResizeArray [| child |] ])
    let inline reactElementWithChildren (name: string) (children: #seq<ReactElement>) =
        reactElement name (createObj [ "children" ==> reactApi.Children.toArray (Array.ofSeq children) ])

    let inline createElement name (properties: IReactProperty list) : ReactElement =
        reactElement name (createObj !!properties)

    let inline createSvgElement name (properties: ISvgAttribute list) : ReactElement =
        reactElement name (createObj !!properties)

    [<Emit "typeof $0 === 'number'">]
    let isTypeofNumber (x: obj) : bool = jsNative


// Types.fs

namespace Feliz

/// Describes an element property
type IReactProperty = interface end

/// Describes style attribute
type IStyleAttribute = interface end

/// Describes an SVG attribute
type ISvgAttribute = interface end 

type ReactElement = Fable.React.ReactElement
type IRefValue<'T> = Fable.React.IRefValue<'T>