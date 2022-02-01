namespace FableTranspiler.Components

open FableTranspiler.VmAdapters.Types

type DtsStatementViewModel =
    {
        DtsStatement: DtsStatementDto
        SelectedDtsStatement: int
    }

module internal DtsStatementViewModel =
    
    let create dto =
        {
            DtsStatement = dto
            SelectedDtsStatement = 0
        }


    type Msg = Msg


    let bindings () = []