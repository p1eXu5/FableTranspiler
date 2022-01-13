module FableTranspiler.Bindings

open Elmish.WPF
open Types
open FableTranspiler.VmAdapters


let rec treeBindings get name =
    name |> Binding.subModelSeq (
        get,
        (fun (vm: FileTreeViewModel) -> vm.Key),
        (fun () -> [
            "IsSelected" |> Binding.twoWay (
                (fun (_, m) -> m.IsSelected),
                (fun v -> ToggleModuleSelection v)
            )
            "FileName" |> Binding.oneWay (fun (_, vm) -> vm.FileName)
            //"SubModules" |> treeBindings (fun m -> m.)
        ])
    )


let rec parentBindings () : Binding<FileTreeViewModel, Msg> list = [
    "IsSelected" |> Binding.twoWay (
        (fun (m) -> m.IsSelected),
        (fun v -> ToggleModuleSelection v)
    )
    "FileName" |> Binding.oneWay (fun (vm) -> vm.FileName)
    "SubModules" |> Binding.subModelSeq (
        (fun m -> m.SubModules),
        (fun (m, sm) -> sm),
        (fun m -> m.Key),
        (fun (id, msg) -> 
            match msg with
            | ToggleModuleSelection _ -> ChildMsg (id, msg)
            | _ -> msg
        ),
        parentBindings
    )
]


let bindings () =
    [
        "OpenFileCommand" |> Binding.cmd (fun m -> ParseFile)

        "ModuleTree" |> Binding.oneWayOpt(fun m -> m.FileTree)

        "ModuleTree2" |> Binding.subModelSeq (
            (fun m -> m.FileTree |> Option.defaultValue []),
            (fun (m, sm) -> sm),
            (fun (vm: FileTreeViewModel) -> vm.Key),
            (fun (id, msg) -> 
                match msg with
                | ToggleModuleSelection _ -> ChildMsg (id, msg)
                | _ -> msg
            ),
            parentBindings
        )

        "SelectedFileTreeModule" |> Binding.subModelSelectedItem (
            "ModuleTree2",
            (fun m -> m.SelectedModuleKey),
            SetSelectedModule
        )

        "SelectedDtsStatements" |> Binding.oneWayOpt (fun m -> 
            match m.FileTree, m.SelectedModuleKey with
            | Some fileTree, Some key ->
                Program.tryFindModule2 key fileTree
                |> Option.bind (fun d ->
                    match d.DtsDocmumentVm.Force() with
                    | Choice1Of2 xvm -> xvm |> Some
                    | Choice2Of2 _ -> None
                )
            | _ -> None            
        )

        "SelectedDtsStatementsError" |> Binding.oneWayOpt(fun m -> 
            match m.FileTree, m.SelectedModuleKey with
            | Some fileTree, Some key ->
                Program.tryFindModule2 key fileTree
                |> Option.bind (fun d ->
                    match d.DtsDocmumentVm.Force() with
                    | Choice1Of2 _ -> None
                    | Choice2Of2 err -> err |> Some
                )
            | _ -> None
        )

        "SelectedFsModule" |> Binding.oneWayOpt(fun m -> 
            m.SelectedDocument
            |> Option.map (fun d ->
                d.FsDocumentVm
            )
        )
        
        "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
    ]