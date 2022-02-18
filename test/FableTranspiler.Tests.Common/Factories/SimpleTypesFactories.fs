namespace FableTranspiler.Tests.Common.SimpleTypesFactories

open FableTranspiler.SimpleTypes


[<RequireQualifiedAccess>]
module FullPath =
    /// Throws Exception when result error.
    let createUnsafe v =
        match FullPath.Create(v) with
        | Ok fp -> fp
        | Error err -> failwith err