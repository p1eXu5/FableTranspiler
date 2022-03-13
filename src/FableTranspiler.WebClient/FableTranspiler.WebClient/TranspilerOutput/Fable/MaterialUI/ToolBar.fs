namespace Fable.MaterialUI

open Fable.Core.JsInterop
open Fable.React.Props
open Fable.Core


type ToolbarProps =
    inherit IHTMLProp

module Toolbar =
    open Fable.React
    
    let inline toolbar props children = domEl (import "Toolbar" "@mui/material/Toolbar") props children