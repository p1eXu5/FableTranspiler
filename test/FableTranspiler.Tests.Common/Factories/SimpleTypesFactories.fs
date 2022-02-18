namespace FableTranspiler.Tests.Common.SimpleTypesFactories

open FableTranspiler.SimpleTypes


[<RequireQualifiedAccess>]
module FullPath =
    /// Throws Exception when result error.
    let createUnsafe v =
        match FullPath.Create(v) with
        | Ok fp -> fp
        | Error err -> failwith err


module ModulePath = 

    let createUnsafe fileName =
        match fileName |> ModulePath.Create with
        | Result.Ok modulePath -> modulePath
        | Result.Error err -> failwith err