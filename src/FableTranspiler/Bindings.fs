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
        "SelectedModule" |> Binding.oneWayOpt(fun m -> 
            m.SelectedDocument
            |> Option.map (fun d ->
                match m.DtsFsTrigger with
                | Dts -> d.DtsDocumentSegmentVmCollection
                | Fs -> d.FsDocumentSegmentVmCollection
            )
        )

        "ShowDtsDocument" 
            |> Binding.twoWay
                (
                    fun m -> 
                        match m.DtsFsTrigger with
                        | Dts -> true
                        | Fs -> false
                    ,
                    (fun v -> if v then SetShowDtsDocument else SetShowFsDocument)
                )

        "ShowFsDocument" 
            |> Binding.twoWay
                (
                    fun m -> 
                        match m.DtsFsTrigger with
                        | Dts -> false
                        | Fs -> true
                    ,
                    (fun v -> if v then SetShowFsDocument else SetShowDtsDocument)
                )
        
        "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
    ]