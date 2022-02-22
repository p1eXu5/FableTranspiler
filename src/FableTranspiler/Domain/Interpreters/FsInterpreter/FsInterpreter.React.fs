module internal rec FableTranspiler.Interpreters.FsInterpreter.React

open FableTranspiler.SimpleTypes
open FableTranspiler.Helpers
open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter.Common
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder
open System
open FableTranspiler.Interpreters.FsInterpreter.FsStatementV2


type Config =
    {
        Namespace: Lazy<Scope>
    }


let private arrayType =
    {
        Identifier = FsStatmentKind.Type
        Scope = Inherit
        Open = []
        CodeItems = [vmPrn "[]"]
        NestedStatements = []
    }

let private optionType =
    {
        Identifier = FsStatmentKind.Type
        Scope = Inherit
        Open = []
        CodeItems = [vmType " option"]
        NestedStatements = []
    }


let rec interpretSingleType (type': DTsType) =
    
    let fsStatement codeItems =
        {
            Identifier = FsStatmentKind.Type
            Scope = Inherit
            Open = []
            CodeItems = codeItems
            NestedStatements = []
        }

    match type' with
    | DTsType.Plain identifiers ->
        let codeItems =
            match identifiers with
            | [identifier] ->
                match identifier |> Identifier.Value with
                | "boolean" -> [vmType "bool"]
                | "number" -> [vmType "float"]
                | typeName -> [vmType typeName]
            | _ -> interpretQualifiers identifiers

        fsStatement codeItems

    | DTsType.Generic (qualifiers, types) ->
        failwith "Not implemented"

    | DTsType.Any -> 
        fsStatement [vmType "obj"]
    | DTsType.Void -> fsStatement [vmType "unit"]

    | DTsType.Typeof qualifiers ->
        failwith "Not implemented"

    | DTsType.Array t ->
        interpretSingleType t + arrayType

    | DTsType.Func (fl, typeDefinition) ->
        let returnType = interpretTypeDefinition typeDefinition
        let parameters = 
            if fl |> List.isEmpty then [FsStatementV2.unitType]
            else fl |> List.map interpretFnParameter
        {
            Identifier = FsStatmentKind.Type
            Scope = Inherit
            Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
            CodeItems =
                [
                    vmPrn "("
                    yield!
                        parameters
                        |> List.map (fun ns -> ns |> codeItems)
                        |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
                    vmPrn " -> "
                    yield! returnType.CodeItems
                    vmPrn ")"
                ]
            
            NestedStatements = []
        }

    | DTsType.Undefined -> FsStatementV2.zeroType
    
    | _ -> failwith "Not implemented"


let interpretTypeDefinition (tdef: TypeDefinition) =
    match tdef with
    | TypeDefinition.Single tn -> interpretSingleType tn
        
    | TypeDefinition.Combination (TypeCombination.Union union) ->
        let types =
            union
            |> List.map interpretSingleType
            |> List.filter FsStatementV2.notZeroType

        if types.Length = 1 then types.Head
        else
            {
                Identifier = FsStatmentKind.Type
                Scope = Inherit
                Open = "Fable.Core" :: (types |> List.map (fun ns -> ns.Open) |> List.concat)
                CodeItems =
                    [
                        vmType $"U{types.Length}"
                        vmPrn "<"
                        yield! 
                            types
                            |> List.map (fun ns -> ns |> codeItems)
                            |> List.reduce (fun t1 t2 -> t1 @ [vmPrn ", "] @ t2)
                        vmPrn ">"
                    ]
            
                NestedStatements = []
            }

    | TypeDefinition.Combination (TypeCombination.Composition dtsTypeList) ->
        failwith "Not implemented"


let rec interpretFnParameter (field, typeDefinition) =

    let fsStatement identifier nested =
        {
            Identifier = FsStatmentKind.Parameter identifier
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = nested
        }

    match field with
    | Field.Required identifier
    | Field.Optional identifier ->
        fsStatement identifier [interpretTypeDefinition typeDefinition]
    | Field.FuncReq (identifier, fl)
    | Field.FuncOpt (identifier, fl) ->
        let returnType = interpretTypeDefinition typeDefinition
        let parameters = 
            if fl |> List.isEmpty then [FsStatementV2.unitType]
            else fl |> List.map interpretFnParameter
        {
            Identifier = FsStatmentKind.Parameter identifier
            Scope = Inherit
            Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
            CodeItems =
                [
                    vmPrn "("
                    yield!
                        parameters
                        |> List.map (fun ns -> ns |> codeItems)
                        |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
                    vmPrn " -> "
                    yield! returnType.CodeItems
                    vmPrn ")"
                ]
            
            NestedStatements = []
        }


let rec internal interpretField (field, typeDefinition) =
    interpreter {
        let! (config: Config, tabLevel) = Interpreter.ask

        let fieldCodeItems identifier =
            [
                tab tabLevel
                vmPrn "| "
                vmText (identifier |> Identifier.Value |> capitalizeFirstLetter); vmKeyword " of "
            ]

        let fsStatement identifier nested =
            {
                Identifier = FsStatmentKind.Field identifier
                Scope = Inherit
                Open = []
                CodeItems = fieldCodeItems identifier
                NestedStatements = nested
            }

        match field with
        | Field.Required identifier
        | Field.Optional identifier ->
            return
                fsStatement identifier [interpretTypeDefinition typeDefinition |> addLineBreak]

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let returnType = interpretTypeDefinition typeDefinition
            let parameters = 
                if fl |> List.isEmpty then [FsStatementV2.unitType]
                else fl |> List.map interpretFnParameter
            return
                {
                    Identifier = FsStatmentKind.Field identifier
                    Scope = Inherit
                    Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
                    CodeItems =
                        [
                            yield! fieldCodeItems identifier
                            vmPrn "("
                            yield!
                                parameters
                                |> List.map (fun ns -> ns |> codeItems)
                                |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
                            vmPrn " -> "
                            yield! returnType.CodeItems
                            vmPrn ")"
                            vmEndLineNull
                        ]
                
                    NestedStatements = []
                }
    }


let internal interpretInterface (interfaceDefinition: InterfaceDefinition) =
    interpreter {
        let! (config: Config, tabLevel) = Interpreter.ask

        match interfaceDefinition with
        | InterfaceDefinition.Plain (identifier, fieldList) ->
            let nestedStatements =
                fieldList
                |> List.map (interpretField >> Interpreter.run (config, tabLevel + 1))
            return
                {
                    Identifier = identifier |> FsStatmentKind.Interface
                    Scope = config.Namespace.Value
                    Open = []
                    CodeItems = [
                        tab tabLevel
                        vmKeyword "type "
                        vmTextS (identifier |> Identifier.Value)
                        vmText "="
                        vmEndLineNull
                    ]
                    NestedStatements = nestedStatements
                }

        | InterfaceDefinition.Extends _ -> return failwith "Not implemented"
    }