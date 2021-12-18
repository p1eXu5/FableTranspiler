module FableTranspiler.Bindings

open Elmish.WPF
open Types
open Implementation


let bindings () =
    [
        "OpenFileCommand" |> Binding.cmd (fun m -> ParseFile)
        "SelectedModule" |> Binding.oneWayOpt(
            (fun m -> m.SelectedModule |> Option.map flowDocumentInterpretator) )
    ]