module Fable.ReactScroll.Scroller
        
open Fable.Core
        
[<Import("unmount", from = @"react-scroll\modules\mixins\scroller")>]
let unmount : unit -> unit = jsNative
        
[<Import("register", from = @"react-scroll\modules\mixins\scroller")>]
let register : name: string -> element: obj -> unit = jsNative
        
[<Import("unregister", from = @"react-scroll\modules\mixins\scroller")>]
let unregister : name: string -> unit = jsNative
        
[<Import("get", from = @"react-scroll\modules\mixins\scroller")>]
let get : name: string -> obj = jsNative
        
[<Import("setActiveLink", from = @"react-scroll\modules\mixins\scroller")>]
let setActiveLink : link: string -> unit = jsNative
        
[<Import("getActiveLink", from = @"react-scroll\modules\mixins\scroller")>]
let getActiveLink : unit -> string = jsNative
        
[<Import("scrollTo", from = @"react-scroll\modules\mixins\scroller")>]
let scrollTo : ``to``: string -> props: obj -> unit = jsNative
        
type Scroller =
    abstract unmount : unit -> unit
    abstract register : name: string -> element: obj -> unit
    abstract unregister : name: string -> unit
    abstract get : name: string -> obj
    abstract setActiveLink : link: string -> unit
    abstract getActiveLink : unit -> string
    abstract scrollTo : ``to``: string -> props: obj -> unit