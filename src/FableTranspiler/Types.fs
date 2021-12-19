module FableTranspiler.Types

open FableTranspiler.Parsers.Types
open Elmish
open Infrastruture

type Model =
    {
        ModuleTree: ModuleTree option
        File: obj
        SelectedModule: Statements option
        IsBusy: bool
        LastError: string option
    }
    with 
        static member Init () =
            {
                ModuleTree = None
                SelectedModule = None
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
    