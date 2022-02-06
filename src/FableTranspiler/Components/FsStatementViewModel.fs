namespace FableTranspiler.Components

open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters.FsInterpreter.Types

[<ReferenceEquality>]
type FsStatementViewModel =
    {
        FsStatement: FsStatementDto
        IsMuted: bool
        SelectedStyle: int
        
    }
    with
        member this.Content() = 
            this.FsStatement.StyledFsStatements[this.SelectedStyle]
                .FsStatement
                |> FsStatement.codeItems

        member this.FsCodeStyle() =
            this.FsStatement.StyledFsStatements[this.SelectedStyle]
                .FsCodeStyle

module internal FsStatementViewModel =
    
    let create dto =
        {
            FsStatement = dto
            IsMuted = false
            SelectedStyle = 0
        }


    type Msg =
        | NextStyle
        | ToggleMute


    let bindings () = []