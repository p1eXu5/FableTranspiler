module internal rec FableTranspiler.VmAdapters.FsDocumentInterpreter

open FableTranspiler.VmAdapters.DocumentSegmentViewModel
open FableTranspiler.Parsers.Types
open System.Linq
open System.Collections.Generic
open System


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


let private interpretSingleType (dict: Dictionary<string, FsDocumentSegmentListViewModel>) (type': DTsType) : DocumentSegmentViewModel list =
    match type' with
    | DTsType.Plain p -> interpretType p
    | DTsType.Any -> [vmType "obj"]
    | DTsType.Void -> [vmKeyword "unit"]
    | DTsType.Typeof (Identifier typeName) ->
        dict[typeName] |> construct
    | _ -> failwith "Not implemented"


let private interpretTypeDefinition (dict: Dictionary<string, FsDocumentSegmentListViewModel>) (tdef: TypeDefinition) : DocumentSegmentViewModel list =
    [
        match tdef with
        | TypeDefinition.Single tn -> 
            yield! interpretSingleType dict tn
        | TypeDefinition.Combination comb ->
            failwith "Not implemented"
            //match comb with
            //| Union l ->
            //    yield! constructCombination " | " l []
            //| Composition l ->
            //    yield!  constructCombination " & " l [] 
    ]



let private interpretFnParameters (dict: Dictionary<string, FsDocumentSegmentListViewModel>) (fields: FieldList) : DocumentSegmentViewModel list =
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
                        yield! interpretTypeDefinition dict td
                        vmPrn ") "
                    ]
                interpret tail (List.append result xvm)

            | _ -> failwith "Not implemented"

        | [] -> result
    
    match fields with
    | [] -> failwith "Fields must not be empty"
    | _ -> interpret fields []


let private interpretField (dict: Dictionary<string, FsDocumentSegmentListViewModel>) (field: Field * TypeDefinition) : DocumentSegmentViewModel list =
    match field with
    | ((Field.Required (Identifier name)), td) -> 
        [
            vmKeyword "abstract "
            vmText name
            //vmPrn ": "
            yield! interpretTypeDefinition dict td
            vmEndLineNull
        ]

    | _ -> failwith "Not implemented"

    


let import name source =
    [
        vmPrn "[<"
        vmType "Import"
        vmPrn "("
        vmText $"\"{name}\", "
        vmKeyword "from"
        vmText $"=\"{source}\""
        vmPrn ")>]"
    ]


let private interpretFn (dict: Dictionary<string, FsDocumentSegmentListViewModel>) keyword name parameters returnType =
    [
        yield vmKeywordS keyword
        yield vmTextS name
        match parameters with
        | [] -> 
            yield vmPrn ": "
            yield vmKeyword "unit"
            yield vmPrn " -> "
        | _ -> 
            yield! interpretFnParameters dict parameters
            yield vmPrn ": "
        yield! interpretTypeDefinition dict returnType
    ]


let private interpretStructure tabLevel fileName (dict: Dictionary<string, FsDocumentSegmentListViewModel>) (structure: StructureStatement) : FsDocumentSegmentListViewModel =

    let import name =
        [
            tab tabLevel
            yield! import name fileName
            vmEndLineNull
        ]

    match structure with
    | FunctionDefinition (FunctionDefinition.Plain ((Identifier name), parameters, returnType)) ->
        (
            name,
            [
                yield! import name
                tab tabLevel
                yield! interpretFn dict "let" name parameters returnType
                vmEndLineNull
                vmEndLineNull
            ],
            (fun () -> interpretFn dict "" "" parameters returnType )
        )
        |> Let

    | InterfaceDefinition (InterfaceDefinition.Plain ((Identifier name), fl)) ->
        (
            name,
            [
                yield! import name
                tab tabLevel
                vmKeyword "type "
                vmTextS name
                vmText "="
                vmEndLineNull
                yield!
                    fl 
                    |> List.map (fun t -> (tab (tabLevel + 1)) :: interpretField dict t)
                    |> List.concat
                vmEndLineNull
            ],
            [ vmType name ]
        )
        |> Typed

    | _ -> failwith "Not implemented"



let toDocumentSegmentViewModelList (fsList: FsDocumentSegmentListViewModel list) : DocumentSegmentViewModel list =
    fsList
    |> List.map segments
    |> List.concat


let toDocumentSegmentVmList ns fileName (dict: Dictionary<string, Dictionary<string, FsDocumentSegmentListViewModel>>) statements =

    let jsModuleName = String( fileName |> Seq.takeWhile ((=) '.' >> not) |> Seq.toArray )

    let rec interpret tabLevel statements (result: FsDocumentSegmentListViewModel list) : FsDocumentSegmentListViewModel list =

        /// append generated view models to the result and invokes interpret
        let continueInterpret tail xvm =
            interpret tabLevel tail (List.append result xvm)

        match statements with
        | head :: tail ->
            match head with
            | Statement.Export (ExportStatement.Structure structure) ->
                let vm = interpretStructure tabLevel jsModuleName (dict[fileName]) structure
                dict[fileName][vm |> name] <- vm
                continueInterpret tail [vm]
            | _ -> continueInterpret tail []

        | [] -> result


    let fsModuleName =
        let capitalized = String( (Char.ToUpper(jsModuleName[0]) :: (jsModuleName |> Seq.skip 1 |> Seq.toList)) |> List.toArray)
        match ns with
        | Some ns' -> $"{ns}.{capitalized}"
        | _ -> capitalized

    let initialResult =
        [
            vmKeyword "module "
            vmText fsModuleName
            vmEndLine null
            vmEndLine null
        ] 
        |> Nameless
        |> List.singleton


    ((interpret 0 statements initialResult  |> toDocumentSegmentViewModelList)  @ [{ Tag = Tag.EndOfDocument; Content = null }]).ToList()