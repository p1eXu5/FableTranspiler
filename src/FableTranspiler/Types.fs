module FableTranspiler.Types

open FableTranspiler.Parsers.Types
open Elmish
open Infrastruture

type Model =
    {
        ModuleTree: ModuleTree option
        SelectedModule: Statements option
        IsBusy: bool
    }
    with 
        static member Init () =
            {
                ModuleTree = None
                SelectedModule = None
                IsBusy = false
            },
            Cmd.none

type Msg =
    | ParseFile
    | FileParsed of Result<ModuleTree, string>
    | Failed of exn
    