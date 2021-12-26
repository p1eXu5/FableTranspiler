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
    }
    with 
        static member Init () =
            {
                FileTree = None
                SelectedDocument = None
                File = null
                IsBusy = false
                LastError = None
            },
            Cmd.none

type Msg =
    | ParseFile
    | FileParsed of Result<ModuleTree, string>
    | Failed of exn
    | SelectFile of obj
    