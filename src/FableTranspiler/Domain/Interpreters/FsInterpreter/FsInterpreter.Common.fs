module internal rec FableTranspiler.Interpreters.FsInterpreter.Common


open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters
open System
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder
open FableTranspiler.SimpleTypes



let interpretQualifiers identifiers : CodeItem list =
    identifiers
    |> List.map (fun t -> [ Identifier.Value(t) |> vmType ])
    |> List.reduce (fun t1 t2 -> 
        [
            yield! t1
            vmText "."
            yield! t2
        ]
    )



let rec interpretSingleType (type': DTsType)  =
    interpreter {
        let! (fsStatementReader: FsStatementReader, _) = Interpreter.ask

        match type' with
        | DTsType.Plain is -> 
            let typeNameVms = interpretQualifiers is
            let typeName = String.Join("", typeNameVms |> List.map (fun t -> t.Content))
            match fsStatementReader.Get ([typeName |> Identifier.Create]) with
            | Some v -> return v |> Choice2Of2
            | None ->
                match typeNameVms[0].Content with
                | "boolean" -> return [vmType "bool"] |> Choice1Of2
                | "number" -> return [vmType "float"] |> Choice1Of2
                | _ -> return typeNameVms |> Choice1Of2

        | DTsType.Generic (qualifiers, types) ->
            return [] |> Choice1Of2

        | DTsType.Any -> return [vmType "obj"] |> Choice1Of2
        | DTsType.Void -> return [vmType "unit"] |> Choice1Of2

        | DTsType.Typeof qualifiers ->
            return fsStatementReader.Get qualifiers |> Option.get |> Choice2Of2

        | DTsType.Array t ->
            match! interpretSingleType t with
            | Choice1Of2 l -> return (l @ [vmPrn "[]"]) |> Choice1Of2
            | _ -> return failwith "Not implemented"

        | DTsType.Func (fl, td) ->
            let! fnType = interpretFnType fl td
            return
                [
                    vmPrn "("
                    yield! fnType
                    vmPrn ")"
                ] 
                |> Choice1Of2

        | DTsType.Undefined -> return [] |> Choice1Of2



        | _ -> return failwith "Not implemented"
    }


let interpretTypeDefinition (tdef: TypeDefinition) =
    interpreter {
        let! (fsStatementReader: FsStatementReader, tabLevel) = Interpreter.ask

        match tdef with
        | TypeDefinition.Single tn -> return! interpretSingleType tn
        | TypeDefinition.Combination (TypeCombination.Union union) ->
            let l = 
                union 
                |> List.filter (function DTsType.Undefined -> false | _ -> true)
                |> List.map (fun dtsType -> interpretSingleType dtsType |> Interpreter.run (fsStatementReader, tabLevel))
                |> List.map (fun t ->
                    match t with
                    | Choice1Of2 l -> l
                    | Choice2Of2 vm -> vm.Construct()
                )
                |> (fun l ->
                    if l.Length > 1 then
                        [
                            vmType $"U{l.Length}"
                            vmPrn "<"
                            yield! 
                                List.reduce (fun l1 l2 -> 
                                    [
                                        yield! l1
                                        vmPrn ", "
                                        yield! l2
                                    ]
                                ) l 
                            vmPrn ">"
                        ]
                    else l.Head
                )

            return l |> Choice1Of2

        | TypeDefinition.Combination (TypeCombination.Composition dtsTypeList) ->
            let l = 
                dtsTypeList 
                |> List.map (fun dtsType -> interpretSingleType dtsType |> Interpreter.run (fsStatementReader, tabLevel))
                |> List.map (fun t ->
                    match t with
                    | Choice1Of2 l -> l @ [vmEndLineNull]
                    | Choice2Of2 vm -> vm.Construct() @ [vmEndLineNull]
                )
                |> List.concat

            return l |> Choice1Of2 
    }


let interpretFnParameterTypes (fields: FieldList) =
    let rec interpret (fields: FieldList) result =
        interpreter {
            match fields with
            | head :: [] ->
                match head with
                | ((Field.Required (Identifier field)), td) -> 
                    let! typeDefinitionInterpretation = interpretTypeDefinition td
                    let xvm = 
                        match typeDefinitionInterpretation with
                        | Choice1Of2 l -> l
                        | Choice2Of2 vm -> vm.Construct()
                        
                    return! interpret [] (List.append result xvm)


                | _ -> return failwith "Not implemented"

            | head :: tail ->
                match head with
                | ((Field.Required (Identifier field)), td) -> 
                    let! typeDefinitionInterpretation = interpretTypeDefinition td
                    let xvm = 
                        [
                            match typeDefinitionInterpretation with
                            | Choice1Of2 l -> yield! l
                            | Choice2Of2 vm -> yield! (vm.Construct())

                            vmPrn " -> "
                        ]
                    return! interpret tail (List.append result xvm)


                | _ -> return failwith "Not implemented"

            | [] -> return result
        }
    
    interpreter {
        match fields with
        | [] -> return [vmKeyword "unit"]
        | _ -> return! interpret fields []
    }


let interpretFnType parameters returnType =
    interpreter {
        let! parametersInterpretation = interpretFnParameterTypes parameters
        let! typeDefinitionInterpretation = interpretTypeDefinition returnType
        return
            [
                match parameters with
                | [] -> 
                    yield vmType "unit"
                    yield vmPrn " -> "
                | _ -> 
                    yield! parametersInterpretation
                    yield vmPrn " -> "
                match typeDefinitionInterpretation with
                | Choice1Of2 l -> yield! l
                | Choice2Of2 vm -> yield! (vm.Construct())
            ]
    }


let interpretFieldFnParameters (fields: FieldList) =
    let rec interpret (fields: FieldList) result =
        interpreter {
            let buildType head tail separator =
                interpreter {
                    match head with
                    | ((Field.Required (Identifier field)), td) -> 
                        let! typeInterpretation = interpretTypeDefinition td
                        let xvm = 
                            [
                                match typeInterpretation with
                                | Choice1Of2 l -> yield! l
                                | Choice2Of2 vm -> yield! (vm.Construct())
                                if separator |> Option.isSome then
                                    vmPrn " -> "
                            ]
                        return! interpret tail (List.append result xvm)

                    | _ -> return failwith "Not implemented"
                }

            match fields with
            | head :: [] -> return! buildType head [] None
            | head :: tail -> return! buildType head tail (Some ())
            | [] -> return result
        }
    
    interpreter {
        match fields with
        | [] -> return [vmType "unit"]
        | _ -> return! interpret fields []
    }

/// Interprets into form `(paramName: ParamType, ...)`.
/// If parameters is empty list then interprets into `()`.
let interpretFnParameters (parameters: FieldList) =

    let rec interpret (parameters: FieldList) result =
        interpreter {
            match parameters with
            | head :: tail ->
                match head with
                | ((Field.Required (Identifier field)), td) -> 
                    let! typeInterpretation = interpretTypeDefinition td
                    let xvm = 
                        [
                            vmPrn "("
                            vmText field
                            vmPrn ": "
                            match typeInterpretation with
                            | Choice1Of2 l -> yield! l
                            | Choice2Of2 vm -> yield! (vm.Construct())
                            vmPrn ") "
                        ]
                    return! interpret tail (List.append result xvm)

                | _ -> return failwith "Not implemented"

            | [] -> return result
        }
    
    interpreter {
        match parameters with
        | [] -> return [vmPrn "()"]
        | _ -> return! interpret parameters []
    }

/// Interprets into two possible options:
///
/// - `<keyword> <fnName> : unit -> <returnType>`
///
/// - `<keyword> <fnName> `<see cref="interpretFnParameters"/>` : <returnType>`
let interpretFn keyword fnName parameters returnType =
    interpreter {
        let! parametersInterpretation = interpretFnParameters parameters
        let! returnTypeInterpretation = interpretTypeDefinition returnType
        return
            [
                yield vmKeywordS keyword
                yield vmTextS fnName
                match parameters with
                | [] -> 
                    yield vmPrn ": "
                    yield vmType "unit"
                    yield vmPrn " -> "
                | _ -> 
                    yield! parametersInterpretation
                    yield vmPrn ": "
                match returnTypeInterpretation with
                | Choice1Of2 l -> yield! l
                | Choice2Of2 vm -> yield! (vm.Construct())
            ]
    }
