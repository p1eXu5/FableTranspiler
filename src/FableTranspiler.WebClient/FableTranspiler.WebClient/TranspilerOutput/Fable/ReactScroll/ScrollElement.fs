module Fable.ReactScroll.ScrollElement

open Fable.React
open Fable.Core
open Fable.Core.JsInterop
open Fable.React.Props

type ScrollElementProps =
    | Name of string
    | Id of string
    interface IHTMLProp


let inline scrollElement<'TProps> (``component``: ReactElementType<'TProps>) =
    fun props children ->
        domEl (importDefault "react-scroll/modules/mixins/scroll-element" ``component``) props children