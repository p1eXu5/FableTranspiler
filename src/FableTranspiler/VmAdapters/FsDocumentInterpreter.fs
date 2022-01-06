module internal rec FableTranspiler.VmAdapters.FsDocumentInterpreter

open FableTranspiler.VmAdapters.DocumentSegmentViewModel
open FableTranspiler.Parsers.Types
open System.Linq


let private interpretType l : DocumentSegmentViewModel list =
    l
    |> List.map (fun t -> [{ Tag = Tag.Type; Content = Identifier.Value(t)}])
    |> List.reduce (fun t1 t2 -> 
        [
            yield! t1
            { Tag = Tag.Text; Content = "." }
            yield! t2
        ]
    )


let private interpretSingleType (type': DTsType) : DocumentSegmentViewModel list =
    match type' with
    | DTsType.Plain p -> interpretType p
    | DTsType.Any -> [vmType "obj"]
    | DTsType.Void -> [vmKeyword "unit"]
    | _ -> failwith "Not implemented"


let private interpretTypeDefinition (tdef: TypeDefinition) : DocumentSegmentViewModel list =
    [
        match tdef with
        | TypeDefinition.Single tn -> 
            yield! interpretSingleType tn
        | TypeDefinition.Combination comb ->
            failwith "Not implemented"
            //match comb with
            //| Union l ->
            //    yield! constructCombination " | " l []
            //| Composition l ->
            //    yield!  constructCombination " & " l [] 
    ]



let private interpretFields (fields: FieldList) : DocumentSegmentViewModel list =
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
                        yield! interpretTypeDefinition td
                        vmPrn ") "
                    ]
                interpret tail (List.append result xvm)

            | _ -> failwith "Not implemented"

        | [] -> result
    
    match fields with
    | [] -> failwith "Fields must not be empty"
    | _ -> interpret fields []



let private interpretStructure tabLevel fileName (structure: StructureStatement) : DocumentSegmentViewModel list =
    match structure with
    | FunctionDefinition (FunctionDefinition.Plain ((Identifier name), parameters, returnType)) ->
        [
            yield tab tabLevel
            yield vmKeyword "let "
            yield vmTextS name
            match parameters with
            | [] -> 
                yield vmPrn ": "
                yield vmKeyword "unit"
                yield vmPrn " -> "
            | _ -> 
                yield! interpretFields parameters
                yield vmPrn ": "
            yield! interpretTypeDefinition returnType
            yield vmEndLine null
        ]
    | _ -> failwith "Not implemented"


let toDocumentSegmentVmList statements fileName ns =

    let rec interpret tabLevel statements result : DocumentSegmentViewModel list =

        /// append generated view models to the result and invokes interpret
        let continueInterpret tail xvm =
            interpret tabLevel tail (List.append result xvm)

        match statements with
        | head :: tail ->
            match head with
            | Statement.Export (ExportStatement.Structure structure) ->
                let xvm = interpretStructure tabLevel fileName structure
                continueInterpret tail xvm
            | _ -> continueInterpret tail []

        | [] -> result @ [{ Tag = Tag.EndOfDocument; Content = null }]


    let moduleName =
        match ns with
        | Some ns' -> $"{ns}.{fileName}"
        | _ -> fileName

    let initialResult =
        [
            vmKeyword "module "
            vmText moduleName
            vmEndLine null
            vmEndLine null
        ]

    (interpret 0 statements initialResult ).ToList()