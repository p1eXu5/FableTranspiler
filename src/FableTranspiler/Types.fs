module FableTranspiler.Types


type CommandAction =
    | GrabFile of (unit -> string)
    | ProcessFile of string

type Command =
    {
        Caption: string
        Action: CommandAction
    }