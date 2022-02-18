namespace FableTranspiler.Adapters.WpfClient.Components

open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter

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
                .FsStatement.CodeItems()

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