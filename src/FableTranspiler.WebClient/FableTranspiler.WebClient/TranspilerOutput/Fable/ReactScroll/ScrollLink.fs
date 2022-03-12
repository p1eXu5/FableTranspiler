module Fable.ReactScroll.ScrollLink

open Fable.React
open Fable.Core
open Fable.React.Props
open Fable.ReactScroll.Scroller

type ScrollLinkProps =
    | Container of Browser.Types.HTMLElement
    interface IHTMLProp


[<ImportDefault("react-scroll/modules/mixins/scroll-link")>]
let private _scrollLink<'TProps> (``component``: ReactElementType<'TProps>) (customScroller: Scroller option) : string = jsNative

let scrollLink<'TProps> (``component``: ReactElementType<'TProps>) (customScroller: Scroller option) =
    fun props children ->
        domEl (_scrollLink ``component`` customScroller) props children