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

type Model =
    {
        SelectedModule: Statement list option
    }

type Msg =
    | ParseFile
    