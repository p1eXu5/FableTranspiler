namespace Fable.ReactScroll

open Fable.Core.JsInterop
open Fable.React.Props

type ElementProps =
    | Name of string
    | Id of string
    interface IHTMLProp

module Element =
    open Fable.React

    let inline element props children = domEl (import "Element" "react-scroll") props children