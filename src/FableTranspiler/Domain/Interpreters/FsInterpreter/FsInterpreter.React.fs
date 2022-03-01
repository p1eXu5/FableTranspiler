module internal rec FableTranspiler.Interpreters.FsInterpreter.React

open FableTranspiler.SimpleTypes
open FableTranspiler.Helpers
open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter.Common
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder
open System
open FableTranspiler.Interpreters.FsInterpreter.FsStatementV2


type InnerInterpretConfig =
    {
        Namespace: Lazy<Scope>
        TryGetLocal: Identifier -> FsStatementV2 option
        TryGetStatement: Identifier list -> FsStatementV2 option
    }


let runFnParameterInterpretation (config, tabLevel)=
    fun (f, td) -> Interpreter.run (config, tabLevel) (interpretFnParameter (f, td))

let runDTsTypeInterpretation (config, tabLevel)=
    fun dtsType -> Interpreter.run (config, tabLevel) (interpretDTsType dtsType)

let rec interpretDTsType (type': DTsType) =
    let fsStatement fsStatmementType codeItems =
        {
            Identifier = FsStatmentKind.Type fsStatmementType
            Scope = Inherit
            Open = []
            CodeItems = codeItems
            NestedStatements = []
        }


    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        match type' with
        | DTsType.Plain identifiers ->

            match identifiers with
            | [identifier] ->
                match identifier |> Identifier.Value with
                | "boolean" -> return fsStatement FsStatementType.Primitive [vmType "bool"]
                | "number" -> return fsStatement FsStatementType.Primitive [vmType "float"]
                | typeName ->
                    match config.TryGetLocal identifier with
                    | Some statement ->
                        return fsStatement (FsStatementType.FieldList identifiers) (statement.NestedStatements |> List.map codeItems |> List.concat)
                    | None -> 
                        return fsStatement (FsStatementType.Unknown typeName) [vmType typeName]
            | _ -> 
                match config.TryGetStatement identifiers with
                | Some statement ->
                    return fsStatement (FsStatementType.FieldList identifiers) (statement.NestedStatements |> List.map codeItems |> List.concat)
                | None -> 
                    let codeItems = interpretQualifiers identifiers
                    return fsStatement (FsStatementType.Unknown ($"%O{codeItems}")) (codeItems)


        | DTsType.Generic (qualifiers, types) ->
            return failwith "Not implemented"

        | DTsType.Any -> 
            return fsStatement FsStatementType.Primitive [vmType "obj"]
        | DTsType.Void -> return unitType

        | DTsType.Typeof qualifiers ->
            return failwith "Not implemented"

        | DTsType.Array t ->
            let! t = interpretDTsType t 
            return t |> FsStatementV2.toArray

        | DTsType.Func (fl, typeDefinition) ->
            let! returnType = interpretTypeDefinition typeDefinition
            let parameters = 
                if fl |> List.isEmpty then [FsStatementV2.unitType]
                else fl |> List.map (runFnParameterInterpretation (config, tabLevel))
            return {
                Identifier = FsStatmentKind.Type FsStatementType.Func
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

        | DTsType.Undefined -> return FsStatementV2.zeroType
    
        | _ -> return failwith "Not implemented"
    }


let interpretTypeCombination combination =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        match combination with
        | TypeCombination.Union union ->
            let types =
                union
                |> List.map (fun t -> Interpreter.run (config, tabLevel) (interpretDTsType t))
                |> List.filter FsStatementV2.notZeroType

            if types.Length = 1 then return types.Head
            else
                return
                    {
                        Identifier = FsStatmentKind.Type FsStatementType.Union
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

        | TypeCombination.Composition dtsTypeList ->
            let types = 
                dtsTypeList 
                |> List.map (runDTsTypeInterpretation (config, tabLevel))

            return
                {
                    Identifier = FsStatmentKind.Type FsStatementType.Composition
                    Scope = Inherit
                    Open = (types |> List.map (fun ns -> ns.Open) |> List.concat)
                    CodeItems = []
                    NestedStatements = types
                }
    }


let interpretTypeDefinition (tdef: TypeDefinition) : Interpreter<InnerInterpretConfig, FsStatementV2> =
    interpreter {
        match tdef with
        | TypeDefinition.Single tn -> return! interpretDTsType tn
        | TypeDefinition.Combination combination -> return! interpretTypeCombination combination 
    }


let rec interpretFnParameter (field, typeDefinition) : Interpreter<InnerInterpretConfig, FsStatementV2> =
    
    let fsStatement identifier nested =
        {
            Identifier = FsStatmentKind.Parameter identifier
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = nested
        }

    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        match field with
        | Field.Required identifier
        | Field.Optional identifier ->
            let! typeDef = interpretTypeDefinition typeDefinition
            return fsStatement identifier [typeDef]
        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let! returnType = interpretTypeDefinition typeDefinition
            let parameters = 
                if fl |> List.isEmpty then [FsStatementV2.unitType]
                else fl |> List.map (runFnParameterInterpretation (config, tabLevel))
            return 
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
    }


let rec internal interpretField (field, typeDefinition) =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

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
            let! td = interpretTypeDefinition typeDefinition
            return
                fsStatement identifier [td |> addLineBreak]

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let! returnType = interpretTypeDefinition typeDefinition
            let parameters = 
                if fl |> List.isEmpty then [FsStatementV2.unitType]
                else fl |> List.map (runFnParameterInterpretation (config, tabLevel))
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
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

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


let internal interpretTypeAlias (typeAlias: TypeAlias) =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        
        match typeAlias with
        | TypeAlias.Plain (identifier, combination) ->
            let! comb = interpretTypeCombination combination
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
                    NestedStatements = [comb]
                }
        | TypeAlias.Generic (identifier, types, combination) -> return failwith "Not implemented"
    }