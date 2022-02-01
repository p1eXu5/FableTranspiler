namespace FableTranspiler

open Elmish
open AppTypes
open FableTranspiler.VmAdapters
open FableTranspiler.Components

type MainModel =
    {
        //FileTree: FileTreeViewModel list option
        //SelectedModuleKey: string list option
        ModuleTreeList: ModuleTreeListViewModel
        //SelectedDocument: FileTreeViewModel option
        IsBusy: bool
        LastError: string option
    }


module internal MainModel =

    type Msg =
        | ModuleTreeMsg of ModuleTreeListViewModel.Msg
        | ParseFile
        | FileParsed of Result<AppTypes.ModuleTreeParsingResult, string>
        | Failed of exn
        | SetSelectedModule of string list option

        
    

    let init () =
        let (moduleTree, msg) = ModuleTreeListViewModel.init ()
        {
            //FileTree = None
            //SelectedModuleKey = None
            ModuleTreeList = moduleTree
            //SelectedDocument = None
            IsBusy = false
            LastError = None
        },
        Cmd.map ModuleTreeMsg msg