module FableTranspiler.Program

open Types
open Implementation
open Elmish

let init () =
    {
        SelectedModule = None
    },
    Cmd.none


let update (msg: Msg) (model: Model) =
    match msg with
    | ParseFile -> 
        {
            model with
                SelectedModule = fakeModule () |> Some
        },
        Cmd.none
