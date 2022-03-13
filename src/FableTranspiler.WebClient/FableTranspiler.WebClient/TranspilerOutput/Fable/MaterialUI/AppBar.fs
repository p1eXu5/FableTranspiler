namespace Fable.MaterialUI

open Fable.Core.JsInterop
open Fable.React.Props
open Fable.Core

[<StringEnum>]
type Position =
    | Fixed
    | Absolute
    | Sticky
    | Static
    | Relative

type AppBarProps =
    | ClassName of string
    | Position of Position
    interface IHTMLProp

module AppBar =
    open Fable.React
    
    let inline appBar props children = domEl (import "AppBar" "@mui/material/AppBar") props children