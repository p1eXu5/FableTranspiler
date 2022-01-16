module internal FableTranspiler.VmAdapters.FsInterpreter.Common


open FableTranspiler.Parsers.Types
open FableTranspiler.VmAdapters
open System




let interpretType l : CodeItemViewModel list =
    l
    |> List.map (fun t -> [ Identifier.Value(t) |> vmType ])
    |> List.reduce (fun t1 t2 -> 
        [
            yield! t1
            vmText "."
            yield! t2
        ]
    )



let rec interpretSingleType (statements: string -> FsStatement option) 
                            (type': DTsType) 
                            : Choice<CodeItemViewModel list, FsStatement> =

    match type' with
    | DTsType.Plain p -> 
        let typeNameVms = interpretType p
        let typeName = String.Join("", typeNameVms |> List.map (fun t -> t.Content))
        match statements typeName with
        | Some v -> v |> Choice2Of2
        | None ->
            match typeNameVms[0].Content with
            | "boolean" -> [vmType "bool"] |> Choice1Of2
            | _ -> typeNameVms |> Choice1Of2

    | DTsType.Any -> [vmType "obj"] |> Choice1Of2
    | DTsType.Void -> [vmKeyword "unit"] |> Choice1Of2

    | DTsType.Typeof (Identifier typeName) ->
        statements typeName |> Option.get |> Choice2Of2

    | DTsType.Array t ->
        match interpretSingleType statements t with
        | Choice1Of2 l -> (l @ [vmPrn "[]"]) |> Choice1Of2
        | _ -> failwith "Not implemented"

    | DTsType.Undefined -> [] |> Choice1Of2

    | _ -> failwith "Not implemented"






let interpretTypeDefinition (statements: string -> FsStatement option) 
                                    (tdef: TypeDefinition) 
                                    : Choice<CodeItemViewModel list, FsStatement> =
    match tdef with
    | TypeDefinition.Single tn -> interpretSingleType statements tn
    | TypeDefinition.Combination (TypeCombination.Union comp) ->
        let l = 
            comp 
            |> List.filter (function DTsType.Undefined -> false | _ -> true)
            |> List.map (interpretSingleType statements)
            |> List.map (fun t ->
                match t with
                | Choice1Of2 l -> l
                | _ -> []
            )
            |> (fun l ->
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
            )

        l |> Choice1Of2

    | TypeDefinition.Combination (TypeCombination.Composition union) ->
            failwith "Not implemented"
            // yield!  constructCombination " & " l [] 






let interpretFnParameterTypes (statements: string -> FsStatement option) (fields: FieldList) : CodeItemViewModel list =
    let rec interpret (fields: FieldList) result =
        match fields with
        | head :: [] ->
            match head with
            | ((Field.Required (Identifier field)), td) -> 
                let xvm = 
                    [
                        match interpretTypeDefinition statements td with
                        | Choice1Of2 l -> yield! l
                        | Choice2Of2 vm -> yield! (vm |> FsStatement.construct)
                    ]
                interpret [] (List.append result xvm)


            | _ -> failwith "Not implemented"

        | head :: tail ->
            match head with
            | ((Field.Required (Identifier field)), td) -> 
                let xvm = 
                    [
                        match interpretTypeDefinition statements td with
                        | Choice1Of2 l -> yield! l
                        | Choice2Of2 vm -> yield! (vm |> FsStatement.construct)

                        vmPrn " -> "
                    ]
                interpret tail (List.append result xvm)


            | _ -> failwith "Not implemented"

        | [] -> result
    
    match fields with
    | [] -> [vmKeyword "unit"]
    | _ -> interpret fields []






let interpretFnType (statements: string -> FsStatement option) parameters returnType =
    [
        match parameters with
        | [] -> 
            yield vmKeyword "unit"
            yield vmPrn " -> "
        | _ -> 
            yield! interpretFnParameterTypes statements parameters
            yield vmPrn " -> "
        match interpretTypeDefinition statements returnType with
        | Choice1Of2 l -> yield! l
        | Choice2Of2 vm -> yield! (vm |> FsStatement.construct)
    ]




let interpretFnParameters (statements: string -> FsStatement option) (fields: FieldList) : CodeItemViewModel list =
    let rec interpret (fields: FieldList) result =
        match fields with
        | head :: tail ->
            match head with
            | ((Field.Required (Identifier field)), td) -> 
                let xvm = 
                    [
                        vmPrn "("
                        vmText field
                        vmPrn ": "
                        match interpretTypeDefinition statements td with
                        | Choice1Of2 l -> yield! l
                        | Choice2Of2 vm -> yield! (vm |> FsStatement.construct)
                        vmPrn ") "
                    ]
                interpret tail (List.append result xvm)

            | _ -> failwith "Not implemented"

        | [] -> result
    
    match fields with
    | [] -> [vmPrn "()"]
    | _ -> interpret fields []


let interpretFn (statements: string -> FsStatement option) keyword name parameters returnType =
    [
        yield vmKeywordS keyword
        yield vmTextS name
        match parameters with
        | [] -> 
            yield vmPrn ": "
            yield vmKeyword "unit"
            yield vmPrn " -> "
        | _ -> 
            yield! interpretFnParameters statements parameters
            yield vmPrn ": "
        match interpretTypeDefinition statements returnType with
        | Choice1Of2 l -> yield! l
        | Choice2Of2 vm -> yield! (vm |> FsStatement.construct)
    ]
