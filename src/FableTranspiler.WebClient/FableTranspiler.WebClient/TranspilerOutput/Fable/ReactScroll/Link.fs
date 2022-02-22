namespace Fable.ReactScroll

open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Fable.React.Props

type ReactScrollLinkProps =
    | To of string
    | Spy of bool
    | Smooth of U2<bool, string>
    | Duration of U3<float, string, (float -> float)>
    interface IHTMLProp

type LinkProps =
    | To of string
    | Spy of bool
    | Smooth of U2<bool, string>
    | Duration of U3<float, string, (float -> float)>
    interface IHTMLProp

module Link =
    open Fable.React

    // or domEl (import "Link" "react-scroll") props children
    let inline link props children = 
        //let foo = importDefault "react-scroll/modules/components/Link"
        //console.log(sprintf "%A" foo)
        //console.log(foo.GetType())
        domEl (importDefault "react-scroll/modules/components/Link") props children