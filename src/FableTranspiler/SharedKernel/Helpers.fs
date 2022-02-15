module FableTranspiler.Helpers

open SimpleTypes
open System.IO

let nesting (LibLocation libPath) (ModulePath modulePath) =
    let mutable path = Path.GetRelativePath(libPath, modulePath)
    let mutable lev = 1
    path <- Path.GetDirectoryName(path)
    while path <> libPath do
        path <- Path.GetDirectoryName(path)
        lev <- lev + 1
    lev