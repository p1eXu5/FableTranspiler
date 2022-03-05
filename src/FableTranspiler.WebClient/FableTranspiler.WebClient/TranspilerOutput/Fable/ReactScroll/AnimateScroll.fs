module Fable.ReactScroll.AnimateScroll

open Fable.Core

type AnimateScroll =
    abstract scrollToBottom : options: obj option -> unit

[<ImportDefault("react-scroll/modules/mixins/animate-scroll")>]
let animateScroll : AnimateScroll = jsNative