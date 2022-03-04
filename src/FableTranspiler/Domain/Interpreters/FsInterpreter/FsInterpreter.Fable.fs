module internal rec FableTranspiler.Interpreters.FsInterpreter.Fable


open System
open FableTranspiler.Helpers
open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters.FsInterpreter.Common
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder



let runFnParameterInterpretation (config, tabLevel)=
    fun (f, td) -> Interpreter.run (config, tabLevel) (interpretFnParameter (f, td))


let runDTsTypeInterpretation (config, tabLevel)=
    fun dtsType -> Interpreter.run (config, tabLevel) (interpretDTsType dtsType)


let rec interpretDTsType (type': DTsType)  : Interpreter< InnerInterpretConfig, FsStatementV2 option * Summary> =
    let fsStatement fsStatmementType codeItems =
        {
            Identifier = FsStatmentKind.Type fsStatmementType
            Scope = Inherit
            Open = []
            CodeItems = codeItems
            NestedStatements = []
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        match type' with
        | DTsType.Plain identifiers ->

            match identifiers with
            | [identifier] ->
                match identifier |> Identifier.Value with
                | "boolean" -> return fsStatement FsStatementType.Primitive [vmType "bool"] |> Some, []
                | "number" -> return fsStatement FsStatementType.Primitive [vmType "float"] |> Some, []
                | typeName ->
                    match config.TryGetLocal identifier with
                    | Some statement ->
                        return fsStatement (FsStatementType.FieldList identifiers) (statement.NestedStatements |> List.map FsStatementV2.codeItems |> List.concat) |> Some, []
                    | None -> 
                        return fsStatement (FsStatementType.Unknown typeName) [vmType typeName] |> Some, []
            | _ -> 
                match config.TryGetStatement identifiers with
                | Some statement ->
                    return fsStatement (FsStatementType.FieldList identifiers) (statement.NestedStatements |> List.map FsStatementV2.codeItems |> List.concat) |> Some, []
                | None -> 
                    let codeItems = interpretQualifiers identifiers
                    return fsStatement (FsStatementType.Unknown ($"%O{codeItems}")) (codeItems) |> Some, []


        | DTsType.Generic _ ->
            return
                None,
                [
                    vmComment $"/// see also %O{type'} "
                    vmEndLineNull
                ]


        | DTsType.Any -> 
            return fsStatement FsStatementType.Primitive [vmType "obj"] |> Some, []
        | DTsType.Void -> return FsStatementV2.unitType |> Some, []

        | DTsType.Typeof qualifiers ->
            return failwith "Not implemented"

        | DTsType.Array dtsType ->
            let! t = interpretDTsType dtsType 
            match fst t with
            | Some s -> return s |> FsStatementV2.toArray |> Some, snd t
            | None -> return None, snd t

        | DTsType.Func (fl, typeDefinition) ->
            let! t = interpretFuncSignature fl typeDefinition (FsStatmentKind.Type FsStatementType.Func) [] []
            return (fst t) |> Some, snd t

        | DTsType.Undefined -> return None, []
    
        | _ -> return failwith "Not implemented"
    }


/// common
let interpretTypeDefinition (tdef: TypeDefinition) : Interpreter<InnerInterpretConfig, (FsStatementV2 option * Summary)> =
    interpreter {
        match tdef with
        | TypeDefinition.Single tn -> return! interpretDTsType tn
        | TypeDefinition.Combination combination -> return! interpretTypeCombination combination 
    }


let interpretTypeCombination combination : Interpreter< InnerInterpretConfig, FsStatementV2 option * Summary> =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        let interpretTypes typeList =
            typeList
            |> List.map (runDTsTypeInterpretation (config, tabLevel))
            |> List.filter (fun t ->
                match fst t with
                | Some s -> FsStatementV2.notZeroType s
                | _ -> true
            )
            |> List.foldBack (fun t state ->
                match fst t with
                | Some s -> (s :: (fst state), snd t @ snd state) 
                | None -> (fst state, snd t @ snd state) ) <| ([],[])


        match combination with
        | TypeCombination.Union union ->
            let (types, summary) = interpretTypes union

            if types.Length = 0 then return None, summary
            elif types.Length = 1 then return types.Head |> Some, summary
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
                                    |> List.map (fun ns -> ns |> FsStatementV2.codeItems)
                                    |> List.reduce (fun t1 t2 -> t1 @ [vmPrn ", "] @ t2)
                                vmPrn ">"
                            ]
            
                        NestedStatements = []
                        PostCodeItems = []
                        Summary = []
                        Hidden = false
                    } |> Some
                    , summary 

        | TypeCombination.Composition dtsTypeList ->
            let (types, summary) = interpretTypes dtsTypeList 

            if not (types |> List.isEmpty) then
                return
                    {
                        Identifier = FsStatmentKind.Type FsStatementType.Composition
                        Scope = Inherit
                        Open = (types |> List.map (fun ns -> ns.Open) |> List.concat)
                        CodeItems = []
                        NestedStatements = types
                        PostCodeItems = []
                        Summary = []
                        Hidden = false
                    } |> Some
                    , summary
            else return None, summary
    }



let interpretFuncSignature fl typeDefinition identifier codeItemPrependix codeItemAppendix =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        let! returnTypeInterpretation = interpretTypeDefinition typeDefinition
        let (returnType, summary) =
            match returnTypeInterpretation with
            | Some s, summary ->
                s, summary
            | None, summary -> 
                FsStatementV2.objType, summary

        let (parameters, summary2) = 
            if fl |> List.isEmpty then [FsStatementV2.unitType], []
            else 
                fl 
                |> List.map (runFnParameterInterpretation (config, tabLevel))
                |> List.foldBack (fun t state -> (fst t :: fst state, snd t @ snd state)) <| ([], [])

        return 
            {
                Identifier = identifier
                Scope = Inherit
                Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
                CodeItems =
                    [
                        yield! codeItemPrependix
                        vmPrn "("
                        yield!
                            parameters
                            |> List.map (fun ns -> ns |> FsStatementV2.codeItems)
                            |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
                        vmPrn " -> "
                        yield! returnType.CodeItems
                        vmPrn ")"
                        yield! codeItemAppendix
                    ]
        
                NestedStatements = []
                PostCodeItems = []
                Summary = []
                Hidden = false
            }, summary @ summary2
    }


let rec interpretFnParameter (field, typeDefinition) : Interpreter<InnerInterpretConfig, (FsStatementV2 * Summary)> =
    
    let fsStatement identifier nested =
        {
            Identifier = FsStatmentKind.Parameter identifier
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = nested
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    interpreter {
        match field with
        | Field.Required identifier
        | Field.Optional identifier ->
            let! typeDef = interpretTypeDefinition typeDefinition
            match typeDef with
            | Some s, summary ->
                return fsStatement identifier [s], summary
            | None, summary -> 
                return fsStatement identifier [FsStatementV2.objType], summary

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            return! interpretFuncSignature fl typeDefinition (FsStatmentKind.Parameter identifier) [] []
    }


let rec interpretField (field, typeDefinition) =
    let fsStatement identifier nested =
        interpreter {
            let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
            let! fieldCodeItems = config.FieldStartWithCodeItems identifier

            return {
                Identifier = FsStatmentKind.Field identifier
                Scope = Inherit
                Open = []
                CodeItems = fieldCodeItems
                NestedStatements = nested
                PostCodeItems = []
                Summary = []
                Hidden = false
            }
        }

    interpreter {
        let! (config: InnerInterpretConfig, _) = Interpreter.ask

        match field with
        | Field.Required identifier
        | Field.Optional identifier ->
            let! td = interpretTypeDefinition typeDefinition
            match fst td with
            | Some s ->
                let! topFsStatement = fsStatement identifier [s |> FsStatementV2.addLineBreak]
                return
                    (topFsStatement, snd td)
            | None -> 
                let! topFsStatement = fsStatement identifier [FsStatementV2.objType |> FsStatementV2.addLineBreak]
                return (topFsStatement, snd td)

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let! fieldCodeItems = config.FieldStartWithCodeItems identifier
            return! interpretFuncSignature fl typeDefinition (FsStatmentKind.Field identifier) (fieldCodeItems) [vmEndLineNull]
    }


let interpretInterface (interfaceDefinition: InterfaceDefinition) =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        let! postCodeItems' = config.InterfacePostCodeItems

        match interfaceDefinition with
        | InterfaceDefinition.Plain (identifier, fieldList) ->
            let nestedStatements =
                fieldList
                |> List.map (interpretField >> Interpreter.run (config, tabLevel + 1))
            return
                {
                    Identifier = identifier |> FsStatmentKind.Interface
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        tab tabLevel
                        vmKeyword "type "
                        vmType (identifier |> Identifier.Value)
                        vmText " ="
                        vmEndLineNull
                    ]
                    NestedStatements = nestedStatements |> List.map fst
                    PostCodeItems = postCodeItems'
                    Summary = nestedStatements |> List.map snd |> List.concat
                    Hidden = false
                }

        | InterfaceDefinition.Extends (identifier, extendedType, fieldList) -> 
            let! (extendedTypeInterpretation, extendedSummary) =
                interpretDTsType extendedType

            let nestedStatements =
                fieldList
                |> List.map (interpretField >> Interpreter.run (config, tabLevel + 1))

            return
                {
                    Identifier = identifier |> FsStatmentKind.Interface
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        tab tabLevel
                        vmKeyword "type "
                        vmType (identifier |> Identifier.Value)
                        vmText " ="
                        vmEndLineNull
                    ]
                    NestedStatements = 
                        extendedTypeInterpretation
                        |> Option.map (fun s ->
                            nestedStatements |> List.map fst |> List.append [s]
                        )
                        |> Option.defaultWith (fun () -> nestedStatements |> List.map fst) 
                    PostCodeItems = postCodeItems'
                    Summary = nestedStatements |> List.map snd |> List.concat |> List.append extendedSummary
                    Hidden = false
                }
    }


let interpretTypeAlias (typeAlias: TypeAlias) =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        let! postCodeItems' = config.InterfacePostCodeItems
        
        match typeAlias with
        | TypeAlias.Plain (identifier, combination) ->
            let! comb = interpretTypeCombination combination
            return
                {
                    Identifier = identifier |> FsStatmentKind.Interface
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        tab tabLevel
                        vmKeyword "type "
                        vmType (identifier |> Identifier.Value)
                        vmText  "="
                        vmEndLineNull
                    ]
                    NestedStatements =
                        match fst comb with
                        | Some s -> [s; ]
                        | None -> []
                    PostCodeItems = postCodeItems'
                    Summary = snd comb
                    Hidden = false
                }
        | TypeAlias.Generic (identifier, types, combination) -> return failwith "Not implemented"
    }


let interpretReactComponent identifier =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        return
            {
                Identifier = identifier |> FsStatmentKind.ReactComponent
                Scope = Scope.Module (Identifier.Value(identifier))
                Open = [
                    "Fable.React"
                    "Fable.Core.JsInterop"
                ]
                CodeItems = [
                    tab tabLevel
                    vmKeyword "let inline "; vmIdentifier identifier; vmText " props children ="; vmEndLineNull
                    tab (tabLevel + 1)
                    vmText "domEl "; vmPrn "("; vmText "importDefault "; vmPrn "\""; vmText config.LibRelativePath.Value; vmPrn "\")"; vmText " props children"; vmEndLineNull
                ]
                NestedStatements = []
                PostCodeItems = []
                Summary = []
                Hidden = false
            }
    }


let interpretConstDefinition constDefinition =
    match constDefinition with
    | ConstDefinition.Const (identifier, td) ->
        interpreter {
            let! (fsStatementOpt, summary) = interpretTypeDefinition td
            return {
                Identifier = FsStatmentKind.Const identifier
                Scope = Scope.Inherit
                Open = []
                CodeItems = []
                NestedStatements = fsStatementOpt |> Option.map List.singleton |> Option.defaultValue []
                PostCodeItems = []
                Summary = summary
                Hidden = true
            }
        }
    | _ -> failwith "Not implemented"


let interpretNamespace identifier fsStatements =
    interpreter {
        return []
    }


let abstractMember identifier =
    interpreter {
        let! (_, tabLevel) = Interpreter.ask
        return [
            tab tabLevel
            vmKeyword "abstract "
            vmIdentifier identifier; vmPrn " : "
        ]
    }  |> Interpreter.addTab


let unionCase identifier =
    interpreter {
        let! (_, tabLevel) = Interpreter.ask
        return [
            tab tabLevel
            vmPrn "| "
            vmText (identifier |> Identifier.Value |> capitalizeFirstLetter); vmKeyword " of "
        ]
    }  |> Interpreter.addTab


let inheritIHTMLProps =
    interpreter {
        let! (_, tabLevel) = Interpreter.ask
        return
            FsStatementV2.htmlPropsInheritance (tabLevel)
    } |> Interpreter.addTab


let emptyInterfacePostCodeItems : Interpreter<InnerInterpretConfig, CodeItem list> =
    interpreter {
        return []
    }


let withAbstractClass interpreter =
    interpreter 
    |> Interpreter.withEnv (fun (c, tabLevel) ->
        { c with
              FieldStartWithCodeItems = abstractMember 
              InterfacePostCodeItems = emptyInterfacePostCodeItems }
        , tabLevel
    )


let withUnion interpreter =
    interpreter 
    |> Interpreter.withEnv (fun (c, tabLevel) ->
        { c with
              FieldStartWithCodeItems = unionCase
              InterfacePostCodeItems = inheritIHTMLProps }
        , tabLevel
    )


let strategy =
    {
        InterpretInterface = interpretInterface
        InterpretTypeAlias = interpretTypeAlias
        InterpretReactComponent = interpretReactComponent
        InterpretConstDefinition = interpretConstDefinition
    }