module internal rec FableTranspiler.VmAdapters.FsDocumentInterpreter

open FableTranspiler.VmAdapters.DocumentSegmentViewModel
open FableTranspiler.VmAdapters.FsDocumentSegmentListViewModel
open FableTranspiler.Parsers.Types
open System.Linq
open System.Collections.Generic
open System


let private interpretType l : DocumentSegmentViewModel list =
    l
    |> List.map (fun t -> [ Identifier.Value(t) |> vmType ])
    |> List.reduce (fun t1 t2 -> 
        [
            yield! t1
            vmText "."
            yield! t2
        ]
    )


let rec private interpretSingleType 
    (dict: Dictionary<string, FsDocumentSegmentListViewModel>) 
    (type': DTsType) 
        : Choice<DocumentSegmentViewModel list, FsDocumentSegmentListViewModel> =

    match type' with
    | DTsType.Plain p -> 
        let typeNameVms = interpretType p
        let typeName = String.Join("", typeNameVms |> List.map (fun t -> t.Content))
        match dict.TryGetValue(typeName) with
        | true, v -> v |> Choice2Of2
        | false, _ -> typeNameVms |> Choice1Of2

    | DTsType.Any -> [vmType "obj"] |> Choice1Of2
    | DTsType.Void -> [vmKeyword "unit"] |> Choice1Of2
    | DTsType.Typeof (Identifier typeName) ->
        dict[typeName] |> Choice2Of2
    | DTsType.Array t ->
        match interpretSingleType dict t with
        | Choice1Of2 l -> (l @ [vmPrn "[]"]) |> Choice1Of2
        | _ -> failwith "Not implemented"
    | _ -> failwith "Not implemented"


let private interpretTypeDefinition 
    (dict: Dictionary<string, FsDocumentSegmentListViewModel>) 
    (tdef: TypeDefinition) 
        : Choice<DocumentSegmentViewModel list, FsDocumentSegmentListViewModel> =
    match tdef with
    | TypeDefinition.Single tn -> 
        interpretSingleType dict tn
    | TypeDefinition.Combination comb ->
        failwith "Not implemented"
        //match comb with
        //| Union l ->
        //    yield! constructCombination " | " l []
        //| Composition l ->
        //    yield!  constructCombination " & " l [] 



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
                        match interpretTypeDefinition dict td with
                        | Choice1Of2 l -> yield! l
                        | Choice2Of2 vm -> yield! (vm |> construct)
                        vmPrn ") "
                    ]
                interpret tail (List.append result xvm)


            | _ -> failwith "Not implemented"

        | [] -> result
    
    match fields with
    | [] -> [vmPrn "()"]
    | _ -> interpret fields []


let private interpretFnParameterTypes (dict: Dictionary<string, FsDocumentSegmentListViewModel>) (fields: FieldList) : DocumentSegmentViewModel list =
    let rec interpret (fields: FieldList) result =
        match fields with
        | head :: [] ->
            match head with
            | ((Field.Required (Identifier field)), td) -> 
                let xvm = 
                    [
                        match interpretTypeDefinition dict td with
                        | Choice1Of2 l -> yield! l
                        | Choice2Of2 vm -> yield! (vm |> construct)
                    ]
                interpret [] (List.append result xvm)


            | _ -> failwith "Not implemented"

        | head :: tail ->
            match head with
            | ((Field.Required (Identifier field)), td) -> 
                let xvm = 
                    [
                        match interpretTypeDefinition dict td with
                        | Choice1Of2 l -> yield! l
                        | Choice2Of2 vm -> yield! (vm |> construct)

                        vmPrn " -> "
                    ]
                interpret tail (List.append result xvm)


            | _ -> failwith "Not implemented"

        | [] -> result
    
    match fields with
    | [] -> [vmKeyword "unit"]
    | _ -> interpret fields []


let private interpretField (dict: Dictionary<string, FsDocumentSegmentListViewModel>) (field: Field * TypeDefinition) : DocumentSegmentViewModel list =
    match field with
    | ((Field.Required (Identifier name)), td) -> 
        [
            vmKeyword "abstract "
            vmText name
            vmPrn " : "
            match interpretTypeDefinition dict td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm |> construct)
            vmEndLineNull
        ]

    | (Field.FuncReq ((Identifier name), fl), td) ->
        [
            vmKeyword "abstract "
            vmTextS name
            yield! interpretFnParameters dict fl
            vmPrn " : "
            match interpretTypeDefinition dict td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm |> construct)
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
        match interpretTypeDefinition dict returnType with
        | Choice1Of2 l -> yield! l
        | Choice2Of2 vm -> yield! (vm |> construct)
    ]


let private interpretFnType (dict: Dictionary<string, FsDocumentSegmentListViewModel>) parameters returnType =
    [
        match parameters with
        | [] -> 
            yield vmKeyword "unit"
            yield vmPrn " -> "
        | _ -> 
            yield! interpretFnParameterTypes dict parameters
            yield vmPrn " -> "
        match interpretTypeDefinition dict returnType with
        | Choice1Of2 l -> yield! l
        | Choice2Of2 vm -> yield! (vm |> construct)
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
            (fun () -> interpretFnType dict parameters returnType )
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

    | ConstDefinition (DeclareConst ((Identifier name), tdef)) ->
        match interpretTypeDefinition dict tdef with
        | Choice1Of2 l -> FsDocumentSegmentListViewModel.Named (name, l)
        | Choice2Of2 vm -> FsDocumentSegmentListViewModel.Link (name, vm)

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
                let vm = 
                    interpretStructure tabLevel jsModuleName (dict[fileName]) structure
                    |> insertAtEnd (vmEndStatement (ref head))

                dict[fileName][vm |> name] <- vm
                continueInterpret tail [vm]

            | Statement.Structure structure ->
                let vm = 
                    interpretStructure tabLevel jsModuleName (dict[fileName]) structure
                    |> insertAtEnd (vmEndStatement (ref head))

                dict[fileName][vm |> name] <- vm
                continueInterpret tail []

            | Statement.Export (ExportStatement.OutDefault (Identifier name)) ->
                continueInterpret tail ([dict[fileName][name]])

            | _ -> continueInterpret tail []

        | [] -> result


    let fsModuleName =
        let name =
            jsModuleName.Split('-')
            |> Seq.map (fun n ->
                String( (Char.ToUpper(n[0]) :: (n |> Seq.skip 1 |> Seq.toList)) |> List.toArray)
            )
            |> fun l -> String.Join("", l |> Seq.toArray)

        match ns with
        | Some ns' -> $"{ns'}.{name}"
        | _ -> name

    let initialResult =
        [
            vmKeyword "module "
            vmText fsModuleName
            vmEndLine null
            vmEndLine null
        ] 
        |> Nameless
        |> List.singleton


    ((interpret 0 statements initialResult  |> toDocumentSegmentViewModelList)  @ [ vmEndDocument ]).ToList()