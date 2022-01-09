module FableTranspiler.Bindings

open Elmish.WPF
open Types
open FableTranspiler.VmAdapters


let bindings () =
    [
        "OpenFileCommand" |> Binding.cmd (fun m -> ParseFile)

        "ModuleTree" |> Binding.oneWayOpt(fun m -> m.FileTree)

        //"ModuleTree" |> Binding.subModelSeq (
        //    (fun m -> m.FileTree |> Option.defaultValue []),
        //    (fun (vm: FileTreeViewModel) -> vm.FileName),
        //    (fun () -> [
        //        "FileName" |> Binding.oneWay (fun (_, vm) -> vm.FileName)
        //    ])
        //)

        //"SelectedFileTreeModule" |> Binding.subModelSelectedItem (
        //    "ModuleTree",
        //    (fun m -> m.SelectedDocument),
        //    SetSelectedFileModuleTree
        //)

        "SelectFile" |> Binding.cmdParam (SelectFile)

        "SelectedDtsModule" |> Binding.oneWayOpt(fun m -> 
            m.SelectedDocument
            |> Option.map (fun d ->
                d.DtsDocumentSegmentVmCollection
            )
        )

        "SelectedFsModule" |> Binding.oneWayOpt(fun m -> 
            m.SelectedDocument
            |> Option.map (fun d ->
                d.FsDocumentSegmentVmCollection
            )
        )
        
        "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
    ]