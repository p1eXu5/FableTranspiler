module Fable.ReactScroll.Helpers

open Fable.Core

type Helpers =
    abstract Scroll : component': obj -> customScroller: obj option -> obj
    abstract Element : component': obj -> obj

[<ImportDefault(@"react-scroll\modules\mixins\Helpers")>]
let helpers : Helpers = jsNative