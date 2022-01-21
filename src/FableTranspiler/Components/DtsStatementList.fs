namespace FableTranspiler.Components

open FableTranspiler.VmAdapters

type DtsStatementList =
    {
        DtsStatements: CodeItem list
        Error: CodeItem list
    }


[<RequireQualifiedAccess>]
module DtsStatementList =

    open Elmish

    let init () =
        {
            DtsStatements = []
            Error = []
        }
        , Cmd.none