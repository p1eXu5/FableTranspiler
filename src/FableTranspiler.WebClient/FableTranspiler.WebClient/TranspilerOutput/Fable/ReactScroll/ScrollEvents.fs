namespace Fable.ReactScroll

open Fable.Core

type ScrollEvent =
    abstract register : eventName: string -> callback: (string -> obj -> unit) -> unit
    abstract remove : eventName: string -> unit

type Events =
    abstract registered : obj
    abstract scrollEvent : ScrollEvent

module ScrollEvents =

    [<ImportDefault("react-scroll/modules/mixins/scroll-events")>]
    let events : Events = jsNative
