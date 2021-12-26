module FableTranspiler.Bindings

open Elmish.WPF
open Types


let bindings () =
    [
        "OpenFileCommand" |> Binding.cmd (fun m -> ParseFile)
        "ModuleTree" |> Binding.oneWayOpt(fun m -> m.FileTree)
        "SelectFile" |> Binding.cmdParam (SelectFile)
        "SelectedModule" |> Binding.oneWayOpt(fun m -> 
            m.SelectedDocument 
            |> Option.map (fun d -> d.DocumentSegmentVmCollection)
        )
        
        "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
    ]