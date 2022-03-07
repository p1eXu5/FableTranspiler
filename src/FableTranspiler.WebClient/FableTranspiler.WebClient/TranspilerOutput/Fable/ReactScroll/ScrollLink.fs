module Fable.ReactScroll.ScrollLink

open Fable.React
open Fable.Core
open Fable.Core.JsInterop
open Fable.React.Props

type ScrollLinkProps =
    | Container of Browser.Types.HTMLElement
    interface IHTMLProp


let inline scrollLink<'TProps> (``component``: ReactElementType<'TProps>) =
    fun props children ->
        domEl (importDefault "react-scroll/modules/mixins/scroll-link" ``component``) props children