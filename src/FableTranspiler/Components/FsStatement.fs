namespace FableTranspiler.Components

open FableTranspiler.VmAdapters

type FsStatement =
    {
        FsStatement: FsStatementDto
        SelectedFsStatement: int
        IsMuted: bool
    }