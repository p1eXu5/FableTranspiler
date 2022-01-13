module FableTranspiler.Types

open Elmish
open Infrastruture
open FableTranspiler.VmAdapters

type Model =
    {
        FileTree: FileTreeViewModel list option
        File: obj
        SelectedDocument: FileTreeViewModel option
        IsBusy: bool
        LastError: string option
        SelectedModuleKey: string list option
    }
    with 
        static member Init () =
            {
                FileTree = None
                SelectedDocument = None
                File = null
                IsBusy = false
                LastError = None
                SelectedModuleKey = None
            },
            Cmd.none


//type ModuleMsg =
//    | ToggleModuleSelection of bool


type Msg =
    | ParseFile
    | FileParsed of Result<ModuleTree, string>
    | Failed of exn
    | SelectFile of obj
    | SetShowDtsDocument
    | SetShowFsDocument
    | ToggleModuleSelection of bool
    | ChildMsg of string list * Msg
    | SetSelectedModule of string list option
    