module internal FableTranspiler.Bindings

open Elmish.WPF
open FableTranspiler.VmAdapters
open FableTranspiler.Components
open FableTranspiler
open FableTranspiler.MainModel

//let rec treeBindings get name =
//    name |> Binding.subModelSeq (
//        get,
//        (fun (vm: FileTreeViewModel) -> vm.Key),
//        (fun () -> [
//            "IsSelected" |> Binding.twoWay (
//                (fun (_, m) -> m.IsSelected),
//                (fun v -> ToggleModuleSelection v)
//            )
//            "FileName" |> Binding.oneWay (fun (_, vm) -> vm.FileName)
//            //"SubModules" |> treeBindings (fun m -> m.)
//        ])
//    )




let bindings () =
    [
        "OpenFileCommand" |> Binding.cmd (fun m -> ParseFile)

        "ModuleTreeListVm" |> Binding.subModel(
            (fun m -> m.ModuleTreeList),
            (fun (_, sm) -> sm),
            ModuleTreeMsg,
            ModuleTreeListViewModel.bindings
        )

        

        //"SelectedFileTreeModule" |> Binding.subModelSelectedItem (
        //    "ModuleTree2",
        //    (fun m -> m.SelectedModuleKey),
        //    SetSelectedModule
        //)

        
        
        "LastError" |> Binding.oneWayOpt (fun m -> m.LastError)
    ]