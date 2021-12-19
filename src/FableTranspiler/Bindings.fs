module FableTranspiler.Bindings

open Elmish.WPF
open Types
open VmAdapters.DocumentSegment
open VmAdapters.FileTree


let bindings () =
    [
        "OpenFileCommand" |> Binding.cmd (fun m -> ParseFile)
        "ModuleTree" |> Binding.oneWayOpt(fun m -> m.ModuleTree |> Option.map (toFileTreeVm >> List.singleton))
        "SelectedModule" |> Binding.oneWayOpt(
            (fun m -> m.SelectedModule |> Option.map toDocumentSegmentVmList) )
    ]