[<RequireQualifiedAccess>]
module internal FableTranspiler.InterpretConfigFactory

open FableTranspiler.Interpreters.FsInterpreter
open System.Collections.Generic
open SimpleTypes
open Infrastruture
open System

let private fableDict = Dictionary<ModulePath, Dictionary<Identifier, FsStatement>>()
let private reactDict = Dictionary<ModulePath, Dictionary<Identifier, FsStatement>>()
let private felizDict = Dictionary<ModulePath, Dictionary<Identifier, FsStatement>>()

let build loggerFactory tryFindModule fsCodeStyle =
    match fsCodeStyle with
    | FsCodeStyle.Fable ->
        raise (NotImplementedException())
        //{
        //    Store = FsStatementInMemoryStore.store fableDict
        //    Interpreters = Fable.interpretators
        //    LoggerFactory = loggerFactory
        //    TryFindModule = tryFindModule
        //}
    | FsCodeStyle.React ->
        raise (NotImplementedException())

    | FsCodeStyle.Feliz ->
        raise (NotImplementedException())

    | _ -> raise (NotImplementedException())