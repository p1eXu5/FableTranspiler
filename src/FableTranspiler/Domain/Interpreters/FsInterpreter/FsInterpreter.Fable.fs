module internal rec FableTranspiler.Interpreters.FsInterpreter.Fable

open FableTranspiler
open FableTranspiler.Helpers
open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Interpreters.FsInterpreter.Common
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder
open FsToolkit.ErrorHandling
open System


let (|Qualifiers|_|) expected (qualifiers: Identifier list) =
    if expected |> List.isEmpty || expected.Length > qualifiers.Length then None
    else
        let rec compare l =
            match l with
            | [] -> true
            | (s, Identifier id) :: tail when s <> id -> false
            | (s, Identifier id) :: tail ->
                compare tail

        if List.zip expected qualifiers |> compare then qualifiers |> List.take (expected.Length) |> Some
        else None


type TypeDefInterpretResult =
    | Type of InnerFsStatement * Summary: Summary
    | Composition of FieldList: TopLevelFsStatement list * UnprocessedStatements: Summary


let rec interpretDTsType (type': DTsType)  : Interpreter< InnerInterpretConfig, (InnerFsStatement * Summary) > =

    let interpretTypeByReference qualifiers =
            match qualifiers with
            | [identifier] ->
                match identifier |> Identifier.value with
                | "boolean" -> InnerFsStatement.primitiveType "bool", []
                | "number" -> InnerFsStatement.primitiveType "float", []
                | "string" -> InnerFsStatement.primitiveType "string", []
                | _ -> InnerFsStatement.reference qualifiers, []
            | _ -> InnerFsStatement.reference qualifiers, []



    interpreter {
        let! (config: InnerInterpretConfig, _) = Interpreter.ask

        match type' with
        | DTsType.Plain qualifiers -> 
            return (interpretTypeByReference qualifiers)

        | DTsType.Generic (qualifiers, typaParameters) ->
            match qualifiers with
            | Qualifiers ["React"; "ComponentType"] ids ->
                let! nestedStatements =
                    typaParameters
                    |> List.map interpretDTsType
                    |> Interpreter.sequence

                return
                    {
                        Type = FsStatementType.ReferenceGeneric ids
                        Open = ["Fable.React"]
                        CodeItems = [
                            vmType "ReactElementType"
                            vmPrn "<"

                        ]
                        PostCodeItems = [vmPrn ">"]
                        NestedStatements = 
                            nestedStatements 
                            |> List.map fst
                            |> List.map (fun inner ->
                                match inner.Type with
                                | FsStatementType.Reference _ ->
                                    {inner with CodeItems = [vmPrn "'"] @ inner.CodeItems}
                                | _ -> inner
                            )

                    }, nestedStatements |> List.map snd |> List.concat
            | Qualifiers ["React"; "ComponentClass"] ids ->
                return
                    {
                        Type = FsStatementType.ReferenceGeneric ids
                        Open = ["Fable.React"]
                        CodeItems = [
                            vmType "ReactElement"
                        ]
                        PostCodeItems = []
                        NestedStatements = []
                    }, []
            | _ ->
                let codeItems = DtsInterpreter.constructSingleType type'
                return
                    {
                        Type = FsStatementType.ReferenceGeneric qualifiers
                        Open = []
                        CodeItems = codeItems
                        PostCodeItems = []
                        NestedStatements = []
                    }, []

        | DTsType.Any -> return InnerFsStatement.objType, []
        | DTsType.Void -> return InnerFsStatement.unitType, []

        | DTsType.Typeof qualifiers -> 
            let matcher statementOption =
                match statementOption with
                | Some statement when statement |> FsStatementV2.isLetImport ->
                    {
                        Type = FsStatementType.FuncSignature
                        CodeItems = []
                        Open = statement.Open
                        NestedStatements = statement.NestedStatements |> List.choose FsStatementV2.inner
                        PostCodeItems = []
                    }, []

                | _ ->
                    InnerFsStatement.reference qualifiers, []

            match qualifiers with
            | [identifier] -> return matcher (config.TryGetLocal identifier)
            | _ ->  return matcher (config.TryGetStatement qualifiers)

        | DTsType.Array dtsType ->
            let! (s, summary) = interpretDTsType dtsType |> InnerInterpretConfig.withDisabledTypeSearching
            return {s with PostCodeItems = s.PostCodeItems @ [vmPrn "[]"] }, summary

        | DTsType.Func (fl, typeDefinition) ->
            let! t = interpretFuncSignature fl typeDefinition [vmPrn "("] [vmPrn ")"]
            return t

        | DTsType.Undefined -> return InnerFsStatement.zeroType, []
    
        | DTsType.InlineObject l when l.Length = 0 ->
            return InnerFsStatement.objType, []

        | DTsType.InlineObject l ->
            /// field is {TopLvel Field {Types}} -> {Anoonymous {InnerField {Types}}}
            let! fields =
                l 
                |> List.map interpretField 
                |> Interpreter.sequence 
                |> Interpreter.map (fun statements ->
                    statements
                    |> List.mapi (fun i s ->
                        let id = s |> TopLevelFsStatement.identifier |> Option.get
                        {
                            Type = FsStatementType.Field (id)
                            Open = []
                            CodeItems = [vmIdentifier id; vmPrn " : "]
                            PostCodeItems = if i < statements.Length - 1 then [vmPrn "; "] else []
                            NestedStatements = s.NestedStatements |> List.choose (function | FsStatementV2.InnerFsStatement s -> Some s | _ -> None) 
                        }
                    )
                )

            return
                {
                    Type = FsStatementType.Anonymous
                    Open = []
                    CodeItems = [vmPrn "{| "]
                    PostCodeItems = [vmPrn " |}"]
                    NestedStatements = fields
                }, []
    }





/// common
let interpretTypeDefinition (tdef: TypeDefinition) : Interpreter<InnerInterpretConfig, TypeDefInterpretResult> =
    interpreter {
        match tdef with
        | TypeDefinition.Single tn -> return! (interpretDTsType tn |> Interpreter.map TypeDefInterpretResult.Type)
        
        | TypeDefinition.Combination combination -> return! interpretTypeCombination combination
    }


let private interpretTypeCombination combination : Interpreter<InnerInterpretConfig, TypeDefInterpretResult> =
    interpreter {
        match combination with
        | TypeCombination.Union union ->
            return! ((interpretUnion union) |> Interpreter.map (fun inner -> TypeDefInterpretResult.Type (inner, [])))
            
        | TypeCombination.Composition dtsTypeList ->
            return! interpretComposition dtsTypeList
    }


let private interpretUnion union : Interpreter< InnerInterpretConfig, InnerFsStatement > =
    interpreter {
        let! types =
            union
            |> List.map interpretDTsType
            |> Interpreter.sequence 
            |> InnerInterpretConfig.wrapFuncWithPrn
            |> InnerInterpretConfig.withFuncSignature namelessFuncSignature
            |> Interpreter.map (List.filter (fun t -> InnerFsStatement.notZeroType (fst t)))

        if types.Length = 0 then return InnerFsStatement.objType
        elif types.Length = 1 then return fst types.Head
        else
            return
                {
                    Type = FsStatementType.Union
                    Open = ["Fable.Core"]
                    CodeItems =
                        [
                            vmType $"U{types.Length}"
                            vmPrn "<"
                        ]
            
                    NestedStatements = 
                        types 
                        |> List.map fst 
                        |> List.mapi (fun i p ->
                            if i = types.Length - 1 then p
                            else {p with PostCodeItems = p.PostCodeItems @ [vmPrn ", "]}
                        )
                    PostCodeItems = [vmPrn ">"]
                }
    }





/// Returns Anonymous or obj type with unprocessed type list
let private anonymousType innerTypes : Interpreter< InnerInterpretConfig, TypeDefInterpretResult> =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        let! fieldStartWith = config.FieldStartWithCodeItems

        let fields =
            //let mapper tabLevelOffset (xns: InnerFsStatement list) =
            //    xns
            //    |> List.map (fun ns ->
            //        {
            //            Type = FsStatementType.Field (ns |> InnerFsStatement.identifier |> List.head)
            //            Scope = Inherit
            //            CodeItems = [
            //                tab (tabLevel + 1)
            //                yield! fieldStartWith (ns |> InnerFsStatement.identifier |> List.head) 
            //            ]

            //        }
            //        {ns with
            //            PostCodeItems = [vmEndLineNull]
            //        } |> Choice1Of3
            //    ) 

            let matcher statementOption =
                match statementOption with
                | Some statement when statement |> FsStatementV2.isInterface ->
                    //let codeItems = statement.NestedStatements |> List.map FsStatementV2.codeItems |> List.concat
                    statement.NestedStatements // fields
                    |> List.choose FsStatementV2.topLevel
                    |> List.map (fun ns ->
                        {ns with 
                            CodeItems = [
                                tab (tabLevel + 1)
                                yield! fieldStartWith (ns |> TopLevelFsStatement.identifier |> Option.get) 
                            ]
                        }
                    )
                    |> Choice1Of3
                | Some statement when statement |> FsStatementV2.isLetImport ->
                    let id = statement |> TopLevelFsStatement.identifier |> Option.get
                    [{
                        Kind = FsStatementKind.Field id
                        Scope = Inherit
                        Open = []
                        CodeItems = [
                            tab (tabLevel + 1)
                            yield! fieldStartWith id 
                        ]
                        NestedStatements = statement.NestedStatements
                        PostCodeItems = [vmEndLineNull]
                        Summary = [] // todo
                        Hidden = false
                    }] |> Choice1Of3

                | Some s -> 
                    Choice2Of3 (s |> FsStatementV2.TopLevelFsStatement)
                | None -> Choice3Of3 ()

            innerTypes
            |> List.map (fun s ->
                match s.Type with
                | FsStatementType.Anonymous ->
                    s.NestedStatements
                    |> List.map (fun ns ->
                        {
                            Kind = FsStatementKind.Field (ns |> InnerFsStatement.identifier |> List.head)
                            Scope = Inherit
                            Open = []
                            CodeItems = [
                                tab (tabLevel + 1)
                                yield! fieldStartWith (ns |> InnerFsStatement.identifier |> List.head) 
                            ]
                            NestedStatements = ns.NestedStatements |> List.map FsStatementV2.InnerFsStatement
                            PostCodeItems = [vmEndLineNull]
                            Summary = [] // todo
                            Hidden = false
                        }
                    )  
                    |> Choice1Of3

                | FsStatementType.Reference qualifiers ->
                    match qualifiers with
                    | [identifier] -> matcher (config.TryGetLocal identifier)
                    | _ ->  matcher (config.TryGetStatement qualifiers)

                | _ -> Choice2Of3 (s |> FsStatementV2.InnerFsStatement)
            )

        let summary = 
            fields 
            |> List.choose (function Choice2Of3 s -> s |> Some | _ -> None)
            |> function
                | [] -> []
                | unprocessedStatements -> seeAlso unprocessedStatements

        match 
            fields 
            |> List.choose (function Choice1Of3 s -> s |> Some | _ -> None) 
            |> List.concat 
        with
        | [] ->
            return (InnerFsStatement.objType, summary) |> TypeDefInterpretResult.Type
        | ns ->
            return
                (ns,  summary) |> TypeDefInterpretResult.Composition
    }


/// Returns Anonymous or obj type with unprocessed type list
let private interpretComposition composition : Interpreter< InnerInterpretConfig, TypeDefInterpretResult> =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        let! types = 
            composition
            |> List.map interpretDTsType
            |> Interpreter.sequence 
            |> InnerInterpretConfig.unwrapFuncWithPrn
            |> Interpreter.map (List.filter (fun t -> InnerFsStatement.notZeroType (fst t)))

        if config.IsTypeSearchEnabled then
            return! anonymousType (types |> List.map fst)
        else
            return (InnerFsStatement.objType, (types |> List.map snd |> List.concat)) |> TypeDefInterpretResult.Type
    }



let namelessFuncSignature (p: InnerFsStatement) =
    {p with PostCodeItems = p.PostCodeItems @ [vmPrn " -> "]}

let namedFuncSignature (p: InnerFsStatement) =
    {p with 
        CodeItems =
            (
                p 
                |> InnerFsStatement.identifier 
                |> (fun xi ->
                    match xi with
                    | [] -> []
                    | [id] -> [vmIdentifier id; vmPrn ": "]
                    | _ -> failwith $"Unexpected identifier: {xi}"
                )
            )
            @ p.CodeItems
        PostCodeItems = p.PostCodeItems @ [vmPrn " -> "]}

let interpretFuncSignature fl returnTypeDefinition codeItemPrependix codeItemAppendix : Interpreter<InnerInterpretConfig, (InnerFsStatement * Summary)> =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        let! (returnType, summary) = 
            interpretTypeDefinition returnTypeDefinition 
            |> InnerInterpretConfig.wrapFuncWithPrn
            |> InnerInterpretConfig.withFuncSignature namelessFuncSignature
            |> Interpreter.map toInnerFsStatementV2Summary


        let! (parameters, summary2) = 
            if fl |> List.isEmpty then Interpreter.retn ([InnerFsStatement.unitType], [])
            else 
                fl 
                |> List.map interpretFnParameter
                |> Interpreter.sequence
                |> Interpreter.map (fun fl' ->
                    fl'
                    |> List.foldBack (fun t state -> (fst t :: fst state, snd t @ snd state)) <| ([], [])
                )

        return 
            {
                Type = FsStatementType.FuncSignature
                Open = []
                CodeItems = codeItemPrependix
        
                NestedStatements =
                    parameters
                    |> List.map (fun p ->
                        config.FuncParameterMapper p
                    )
                    |> List.append <| [returnType]
                
                PostCodeItems = codeItemAppendix
            }, summary @ summary2
    }


//let interpretNamedFuncSignature fl typeDefinition kind codeItemPrependix codeItemAppendix =
//    interpreter {
//        let! returnTypeInterpretation = interpretTypeDefinition typeDefinition |> InnerInterpretConfig.wrapFuncWithPrn<TopLevelFsStatement option * Summary>
//        let (returnType, summary) =
//            match returnTypeInterpretation with
//            | Some s, summary ->
//                s, summary
//            | None, summary -> 
//                FsStatementV2.objType, summary

//        let! (parameters, summary2) = 
//            if fl |> List.isEmpty then Interpreter.retn ([FsStatementV2.unitType], [])
//            else 
//                fl 
//                |> List.map interpretFnParameter
//                |> Interpreter.sequence
//                |> Interpreter.map (fun fl' ->
//                    fl'
//                    |> List.foldBack (fun t state -> (fst t :: fst state, snd t @ snd state)) <| ([], [])
//                )
//                |> InnerInterpretConfig.wrapFuncWithPrn

//        return 
//            {
//                Kind = kind
//                Scope = Inherit
//                Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
//                CodeItems = codeItemPrependix
//                NestedStatements = [
//                    {
//                        Kind = kind
//                        Scope = Inherit
//                        Open = []
//                        CodeItems =
//                            [
//                                yield!
//                                    parameters
//                                    |> List.map (fun ns -> 
//                                    (
//                                        ns |> FsStatementV2.identifier
//                                        |> Option.map (fun id ->
//                                            [
//                                                vmIdentifier id
//                                                vmPrn ": "
//                                            ]
//                                        )
//                                        |> Option.defaultValue []
//                                    )
//                                     @ (ns |> FsStatementV2.codeItems))
//                                    |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
//                                vmPrn " -> "
//                                yield! returnType |> FsStatementV2.codeItems
//                            ]
        
//                        NestedStatements = []
//                        PostCodeItems = []
//                        Summary = []
//                        Hidden = false
//                    }
//                ]
//                PostCodeItems = codeItemAppendix
//                Summary = summary @ summary2
//                Hidden = false
//            }, summary @ summary2
//    }


/// Return {Parameter {TypeDef}}
let rec interpretFnParameter (field, typeDefinition) : Interpreter<InnerInterpretConfig, (InnerFsStatement * Summary)> =
    
    let fsStatement identifier nested =
        {
            Type = FsStatementType.Parameter identifier
            Open = []
            CodeItems = []
            NestedStatements = [nested]
            PostCodeItems = []
        }

    interpreter {
        let! (config: InnerInterpretConfig, _) = Interpreter.ask

        match field with
        | Field.Required identifier ->
            let! (typeDef, summary) = 
                interpretTypeDefinition typeDefinition
                |> InnerInterpretConfig.withFuncSignature namelessFuncSignature
                |> Interpreter.map toInnerFsStatementV2Summary
                
            return fsStatement identifier typeDef, summary

        | Field.Optional identifier ->
            let! (typeDef, summary) = 
                interpretTypeDefinition typeDefinition
                |> InnerInterpretConfig.withFuncSignature namelessFuncSignature
                |> Interpreter.map toInnerFsStatementV2Summary

            return fsStatement identifier {typeDef with PostCodeItems = typeDef.PostCodeItems @ [vmType " option"]}, summary

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let! (signature, summary) = 
                interpretFuncSignature fl typeDefinition [vmPrn "("] [vmPrn ")"]
                |> InnerInterpretConfig.withFuncSignature namelessFuncSignature
            return (fsStatement identifier signature, summary)
    }


let private identifierCodeItemList (fsStatements: FsStatementV2 list) =
    fsStatements
    |> List.map FsStatementV2.identifier
    |> List.map interpretQualifiers
    |> function
        | [] -> []
        | l -> l |> List.reduce (fun l1 l2 -> l1 @ [vmPrn ", "] @ l2)


let private seeAlso unprocessedStatements : Summary =
    [vmComment "/// see also "] @ (unprocessedStatements |> List.map FsStatementV2.CollectCodeItems |> List.concat ) @ [vmEndLineNull]


let rec interpretField (field, typeDefinition) : Interpreter<InnerInterpretConfig, TopLevelFsStatement> =
    interpreter {
        let! (config: InnerInterpretConfig, _) = Interpreter.ask
        let! fieldCodeItems = config.FieldStartWithCodeItems

        let fieldStatement identifier nested summary =
            {
                Kind = FsStatementKind.Field identifier
                Scope = Inherit
                Open = []
                CodeItems = fieldCodeItems identifier
                NestedStatements = nested
                PostCodeItems = [vmEndLineNull]
                Summary = summary
                Hidden = false
            }

        match field with
        | Field.Required identifier
        | Field.Optional identifier ->
            let! (td, summary) = 
                interpretTypeDefinition typeDefinition
                // |> InnerInterpretConfig.withFuncSignature namedFuncSignature
                |> Interpreter.map toFsStatementV2Summary

            return fieldStatement identifier td summary

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let! (funcSignature, summary) = 
                interpretFuncSignature fl typeDefinition [] [] 
                // |> InnerInterpretConfig.withFuncSignature namedFuncSignature

            return fieldStatement identifier [funcSignature |> FsStatementV2.InnerFsStatement] summary
    }


let interpretInterface (interfaceDefinition: InterfaceDefinition) : Interpreter< InnerInterpretConfig, TopLevelFsStatement > =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        let! postCodeItems' = config.InterfacePostCodeItems

        match interfaceDefinition with
        | InterfaceDefinition.Plain (identifier, fieldList) ->
            let! nestedStatements =
                fieldList
                |> List.map interpretField
                |> Interpreter.sequence
                |> Interpreter.addTab

            return
                {
                    Kind = identifier |> config.InterfaceStatementKind
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        tab tabLevel
                        vmKeyword "type "
                        vmType (identifier |> Identifier.value)
                        vmText " ="
                        vmEndLineNull
                    ]
                    NestedStatements = nestedStatements |> List.map FsStatementV2.TopLevelFsStatement
                    PostCodeItems = postCodeItems'
                    Summary = []
                    Hidden = false
                }

        | InterfaceDefinition.Extends (identifier, extendedType, fieldList) -> 
            let! (extendedTypeInterpretation, summary) =
                interpretDTsType extendedType
                |> Interpreter.bind (fun s -> 
                    interpreter {
                        match! anonymousType [fst s] with
                        | TypeDefInterpretResult.Type (_, summary) ->
                            return None, summary
                        | TypeDefInterpretResult.Composition (fieldList, summary) ->
                            return Some fieldList, summary
                    }
                )
                

            let! nestedStatements =
                fieldList
                |> List.map interpretField
                |> Interpreter.sequence
                |> Interpreter.addTab

            return
                {
                    Kind = identifier |> FsStatementKind.DU
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        tab tabLevel
                        vmKeyword "type "
                        vmType (identifier |> Identifier.value)
                        vmText " ="
                        vmEndLineNull
                    ]
                    NestedStatements = 
                        extendedTypeInterpretation
                        |> Option.defaultValue []
                        |> List.map FsStatementV2.TopLevelFsStatement
                        |> List.append
                        <| (nestedStatements |> List.map FsStatementV2.TopLevelFsStatement)
                    
                    PostCodeItems = postCodeItems'
                    Summary = summary
                    Hidden = false
                }
    }

/// Wraps InnerFsStatement with FsStatementV2 and returns it with Summary
let private toFsStatementV2Summary typeDef : (FsStatementV2 list * Summary) =
    match typeDef with
    | TypeDefInterpretResult.Type (inner, summary) -> [(inner |> FsStatementV2.InnerFsStatement)], summary
    | TypeDefInterpretResult.Composition (topLevel, summary) ->
        (topLevel |> List.map FsStatementV2.TopLevelFsStatement), summary


let private toInnerFsStatementV2Summary typeDef : (InnerFsStatement * Summary) =
    match typeDef with
    | TypeDefInterpretResult.Type (inner, summary) -> inner, summary
    | TypeDefInterpretResult.Composition (topLevels, summary) ->
        failwith "Unexpected behavior"


let interpretTypeAlias (typeAlias: TypeAlias) =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        let! postCodeItems' = config.InterfacePostCodeItems
        
        match typeAlias with
        | TypeAlias.Generic (identifier, _, combination)
        | TypeAlias.Plain (identifier, combination) ->
            let! (nested, summary) = 
                interpretTypeCombination combination
                |> InnerInterpretConfig.withEnableTypeSearching
                |> Interpreter.map toFsStatementV2Summary

            return
                {
                    Kind = identifier |> config.InterfaceStatementKind
                    Scope = Scope.Namespace
                    Open = []
                    CodeItems = [
                        tab tabLevel
                        vmKeyword "type "
                        vmType (identifier |> Identifier.value)
                        vmText  " ="
                        vmEndLineNull
                    ]
                    NestedStatements = nested
                    PostCodeItems = postCodeItems'
                    Summary = summary
                    Hidden = false
                }
    }


let interpretReactComponent identifier =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        return
            {
                Kind = identifier |> FsStatementKind.ReactComponent
                Scope = Scope.Module (ModuleScope.Nested (Identifier.value(identifier)))
                Open = [
                    "Fable.React"
                    "Fable.Core.JsInterop"
                ]
                CodeItems = [
                    tab tabLevel
                    vmKeyword "let inline "; vmIdentifier identifier; vmText " props children ="; vmEndLineNull
                    tab (tabLevel + 1)
                    vmText "domEl "; vmPrn "("; vmText "importDefault "; vmPrn "@\""; vmPrn config.LibRelativePath.Value; vmPrn "\")"; vmText " props children"; vmEndLineNull
                ]
                NestedStatements = []
                PostCodeItems = []
                Summary = []
                Hidden = false
            }
    }


let interpretConstDefinition constDefinition =
    match constDefinition with
    | ConstDefinition.DeclareConst (identifier, td)
    | ConstDefinition.Const (identifier, td) ->
        interpreter {
            let! (nested, summary) = 
                interpretTypeDefinition td 
                |> InnerInterpretConfig.withDisabledTypeSearching
                |> Interpreter.map toFsStatementV2Summary

            return {
                Kind = FsStatementKind.Const identifier
                Scope = Scope.Inherit
                Open = []
                CodeItems = []
                NestedStatements = nested
                PostCodeItems = []
                Summary = summary
                Hidden = true
            }
        }


let interpretNamespace identifier namespaceFsStatements =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
        let! fieldCodeItems = config.FieldStartWithCodeItems |> Interpreter.addTab

        let fieldFsStatements = 
            namespaceFsStatements 
            |> List.choose (fun s -> 
                match s.Kind with
                | FsStatementKind.Const id ->
                    {
                        Kind = FsStatementKind.Field id
                        Scope = Scope.Inherit
                        Open = []
                        CodeItems = fieldCodeItems id
                        NestedStatements = s.NestedStatements |> List.map (FsStatementV2.addLineBreak)
                        PostCodeItems = []
                        Summary = s.Summary
                        Hidden = false
                    } |> Some
                | _ -> None
            )

        if fieldFsStatements |> List.isEmpty then
            return None
        else
            let nsTypeFsStatement =
                {
                    Kind = FsStatementKind.AbstractClass identifier
                    Scope = Scope.Inherit
                    Open = []
                    CodeItems = [
                        vmKeyword "type "
                        vmTypeIdentifier identifier
                        vmPrn " ="
                        vmEndLineNull
                    ]
                    NestedStatements = fieldFsStatements |> List.map FsStatementV2.TopLevelFsStatement
                    PostCodeItems = []
                    Summary = []
                    Hidden = false
                }

            return nsTypeFsStatement |> Some
    }


let interpretFunctionDefinition functionDefinition =
    let isGenericType typeName = function
        | Single t -> 
            match t with
            | DTsType.Generic (qualifiers, _) ->
                qualifiers |> List.last |> Identifier.value |> (=) typeName
            | _ -> false
        | _ -> false

    let isReactComponenetType (parameters: FieldList) =
        parameters.Length = 1
        && parameters.Head 
            |> snd 
            |> isGenericType "ComponentType"

    let isReactComponenetClass = isGenericType "ComponentClass"

    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        match functionDefinition with
        | FunctionDefinition.Plain (identifier, fl, retType) ->
            let! (signature, summary) = 
                interpretFuncSignature fl retType  [] [] 
                |> InnerInterpretConfig.withFuncSignature namedFuncSignature

            return
                {
                    Kind = FsStatementKind.LetImport identifier
                    Scope = Scope.Module (ModuleScope.Main)
                    Open = ["Fable.Core"]
                    CodeItems = [
                        vmPrn "[<"; vmText "Import"; vmPrn $"(\"{Identifier.value identifier}\", "; vmText "from="; vmPrn $"@\"{config.LibRelativePath.Value}\")>]"; vmEndLineNull
                        vmKeyword "let "; vmIdentifier (identifier |> Identifier.map Helpers.uncapitalizeFirstLetter); vmPrn " : "
                    ]
                    NestedStatements = [signature |> FsStatementV2.InnerFsStatement]
                    PostCodeItems = [
                        vmPrn " = "
                        vmText "jsNative"
                        vmEndLineNull
                    ]
                    Summary = summary
                    Hidden = false
                }
        | FunctionDefinition.GenericNameless (_, parameters, returnType) when parameters |> isReactComponenetType && returnType |> isReactComponenetClass ->
            let funcName = config.LibRelativePath.Value |> Helpers.toModuleName Helpers.uncapitalizeFirstLetter
            return
                {
                    Kind = FsStatementKind.Let (funcName |> Identifier.create)
                    Scope = Scope.Module (ModuleScope.Main)
                    Open = ["Fable.React"; "Fable.Core.JsInterop"; "Fable.React.Props"]
                    CodeItems = [
                        vmKeyword "let inline "
                        vmText funcName
                        vmPrn "<"; vmType "'TProps"; vmPrn "> (``"; vmText "component"; vmPrn "``: "; vmType "ReactElementType"; vmPrn "<"; vmType "'TProps"; vmPrn ">) ="
                        vmEndLineNull
                        tab (tabLevel + 1)
                        vmKeyword "fun "
                        vmText "props children"
                        vmPrn " ->"; vmEndLineNull
                        tab (tabLevel + 2)
                        vmText "domEl "; vmPrn "("
                        vmText "importDefault "
                        vmPrn $"@\"{config.LibRelativePath.Value}\""
                        vmText " ``component``"; vmPrn ")"
                        vmText " props children"
                    ]
                    NestedStatements = []
                    PostCodeItems = []
                    Summary = [vmComment $"/// factory of %s{returnType |> DtsInterpreter.constructTypeDefinition |> CodeItem.toString}"; vmEndLineNull]
                    Hidden = false
                }

        | _ -> return failwith $"{functionDefinition} interpretation is not implemented"
    }


// ==============================================================


let abstractMember =
    interpreter {
        let! (_, tabLevel) = Interpreter.ask
        return fun identifier -> [
            tab tabLevel
            vmKeyword "abstract "
            vmIdentifier identifier; vmPrn " : "
        ]
    }


let unionCase =
    interpreter {
        let! (_, tabLevel) = Interpreter.ask
        return fun identifier -> [
            tab tabLevel
            vmPrn "| "
            vmText (identifier |> Identifier.value |> capitalizeFirstLetter); vmKeyword " of "
        ]
    }


let parameter : Interpreter<InnerInterpretConfig, Identifier -> CodeItem list> =
    interpreter {
        return fun identifier -> [
            vmIdentifier identifier; vmPrn " : "
        ]
    }


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
              InterfacePostCodeItems = emptyInterfacePostCodeItems 
              InterfaceStatementKind = FsStatementKind.AbstractClass
              FuncParameterMapper = namedFuncSignature
        }
        , tabLevel
    )


let withUnion interpreter =
    interpreter 
    |> Interpreter.withEnv (fun (c, tabLevel) ->
        { c with
              FieldStartWithCodeItems = unionCase
              InterfacePostCodeItems = inheritIHTMLProps
              InterfaceStatementKind = FsStatementKind.DU
              FuncParameterMapper = namelessFuncSignature
        }
        , tabLevel
    )

let strategy =
    {
        InterpretInterface = interpretInterface
        InterpretTypeAlias = interpretTypeAlias
        InterpretReactComponent = interpretReactComponent
        InterpretConstDefinition = interpretConstDefinition
        InterpretNamespace = interpretNamespace
        InterpretFunctionDefinition = interpretFunctionDefinition
    }