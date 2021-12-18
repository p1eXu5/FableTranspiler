module FableTranspiler.Types


type CommandAction =
    | GrabFile of (unit -> string)
    | ProcessFile of string

type Command =
    {
        Caption: string
        Action: CommandAction
    }




open FableTranspiler.Parsers.Types
open Elmish

type Model =
    {
        SelectedModule: Statements option
        IsBusy: bool
    }
    with 
        static member Init () =
            {
                SelectedModule = None
                IsBusy = false
            },
            Cmd.none

type Msg =
    | ParseFile
    | FileParsed of Result<Statements, string>
    | Failed of exn
    