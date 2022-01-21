namespace FableTranspiler.Components


type ModuleTree =
    {
        SelectedFsModule: FsStatementList
        SelectedDtsModule: DtsStatementList
    }


[<RequireQualifiedAccess>]
module ModuleTree =

    open Elmish

    let init () =
        let (selectedFsModule, fsModuleMsg) = FsStatementList.init ()
        let (selectedDtsModule, dtsModuleMsg) = DtsStatementList.init ()
        {
            SelectedFsModule = selectedFsModule
            SelectedDtsModule = selectedDtsModule
        }
        , Cmd.batch [fsModuleMsg; dtsModuleMsg]