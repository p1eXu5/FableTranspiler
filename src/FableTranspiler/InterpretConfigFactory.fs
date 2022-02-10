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

let build loggerFactory fsCodeStyle =
    match fsCodeStyle with
    | FsCodeStyle.Fable ->
        {
            Store = FsStatementInMemoryStore.store fableDict
            Interpreters = Fable.interpretators
            LoggerFactory = loggerFactory
        }
    | FsCodeStyle.React ->
        raise (NotImplementedException())

    | FsCodeStyle.Feliz ->
        raise (NotImplementedException())

    | _ -> raise (NotImplementedException())