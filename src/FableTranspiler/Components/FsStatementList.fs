namespace FableTranspiler.Components

open FableTranspiler.VmAdapters


type FsStatementList =
    {
        FsTatements: FsStatement list
        Error: CodeItem list
    }

type Msg =
    | SelectFsStatement


[<RequireQualifiedAccess>]
module FsStatementList =

    open Elmish

    let init () =
        {
            FsTatements = []
            Error = []
        }
        , Cmd.none


    let update msg model =
        ()