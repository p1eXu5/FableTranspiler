module internal rec FableTranspiler.Interpreters.FsInterpreter.Fable

open FableTranspiler
open FableTranspiler.Helpers
open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Interpreters.FsInterpreter.Common
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder
open System

/// run <see cref="interpretFnParameter(Field*TypeDefinition)" />
let runFnParameterInterpretation (config, tabLevel)=
    fun (f, td) -> Interpreter.run (config, tabLevel) (interpretFnParameter (f, td))


let runDTsTypeInterpretation (config, tabLevel)=
    fun dtsType -> Interpreter.run (config, tabLevel) (interpretDTsType dtsType)


let rec interpretDTsType (type': DTsType)  : Interpreter< InnerInterpretConfig, TopLevelFsStatement option * Summary> =
    let fsStatement fsStatmementType (nestedStatements: TopLevelFsStatement list) =
        {
            Kind = FsStatementKind.Type fsStatmementType
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = 
                if (nestedStatements.Length = 1) && nestedStatements.Head.Kind = FsStatementKind.Type FsStatementType.Composition then
                    nestedStatements.Head.NestedStatements
                else
                    nestedStatements
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    let interpretTypeByReference qualifiers =
        interpreter {
            let! (config: InnerInterpretConfig, _) = Interpreter.ask

            match qualifiers with
            | [identifier] ->
                match identifier |> Identifier.value with
                | "boolean" -> return FsStatementV2.primitiveType "bool" |> Some, []
                | "number" -> return FsStatementV2.primitiveType "float" |> Some, []
                
                | typeName when config.IsTypeSearchEnabled ->
                    match config.TryGetLocal identifier with
                    | Some statement ->
                        //let codeItems = statement.NestedStatements |> List.map FsStatementV2.codeItems |> List.concat
                        return fsStatement (FsStatementType.FieldList qualifiers) statement.NestedStatements |> Some, []
                    | None -> 
                        return FsStatementV2.reference typeName |> Some, []

                | typeName (* when not config.IsTypeSearchEnabled *) ->
                    return FsStatementV2.reference typeName |> Some, []

            | _  when config.IsTypeSearchEnabled -> 
                match config.TryGetStatement qualifiers with
                | Some statement ->
                    return fsStatement (FsStatementType.FieldList qualifiers) (statement.NestedStatements) |> Some, []
                | None -> 
                    let typeName = String.Join("", qualifiers |> List.map Identifier.value)
                    return FsStatementV2.reference typeName |> Some, []
            | _ -> 
                let typeName = String.Join("", qualifiers |> List.map Identifier.value)
                return FsStatementV2.reference typeName |> Some, []
        }



    interpreter {
        let! (config: InnerInterpretConfig, _) = Interpreter.ask

        match type' with
        | DTsType.Plain qualifiers ->
            return! interpretTypeByReference qualifiers


        | DTsType.Generic _ ->
            return
                None,
                [
                    vmComment $"/// see also %O{type'} "
                    vmEndLineNull
                ]


        | DTsType.Any -> 
            return FsStatementV2.objType |> Some, []
        | DTsType.Void -> return FsStatementV2.unitType |> Some, []

        | DTsType.Typeof qualifiers ->
            return! (interpretTypeByReference qualifiers)

        | DTsType.Array dtsType ->
            let! t = interpretDTsType dtsType 
            match fst t with
            | Some s -> return s |> FsStatementV2.toArray |> Some, snd t
            | None -> return None, snd t

        | DTsType.Func (fl, typeDefinition) when config.WrapFuncWithPrn ->
            let! t = interpretFuncSignature fl typeDefinition (FsStatementKind.Type FsStatementType.Func) [vmPrn "("] [vmPrn ")"]
            return (fst t) |> Some, snd t

        | DTsType.Func (fl, typeDefinition) (* when not config.WrapFuncWithPrn *) ->
            let! t = interpretFuncSignature fl typeDefinition (FsStatementKind.Type FsStatementType.Func) [] []
            return (fst t) |> Some, snd t

        | DTsType.Undefined -> return None, []
    
        | DTsType.InlineObject l when l.Length = 0 ->
            return FsStatementV2.objType |> Some, []

        | DTsType.InlineObject l ->
            let! fields =
                l |> List.map interpretField |> Interpreter.sequence |> Interpreter.withEnv (fun (c, t) -> {c with FieldStartWithCodeItems = anonymous}, t)

            return
                {
                    Kind = FsStatementKind.Type FsStatementType.Anonymous
                    Scope = Inherit
                    Open = []
                    CodeItems = [vmPrn "{| "]
                    PostCodeItems = [vmPrn " |}"]
                    NestedStatements = fields |> List.map fst
                    Summary = []
                    Hidden = false
                } |> Some, fields |> List.map snd |> List.concat

        | _ -> return failwith $"%A{type'} interpretation is not implemented."
    }


/// common
let interpretTypeDefinition (tdef: TypeDefinition) : Interpreter<InnerInterpretConfig, (TopLevelFsStatement option * Summary)> =
    interpreter {
        match tdef with
        | TypeDefinition.Single tn -> return! interpretDTsType tn
        | TypeDefinition.Combination combination -> return! interpretTypeCombination combination 
    }


let interpretTypeCombination combination : Interpreter< InnerInterpretConfig, TopLevelFsStatement option * Summary> =

    let interpretTypes typeList =
        interpreter {
            let! typeListInterpretation =
                typeList
                |> List.map interpretDTsType
                |> Interpreter.sequence

            return
                typeListInterpretation
                |> List.filter (fun t ->
                    match fst t with
                    | Some s -> FsStatementV2.notZeroType s
                    | _ -> true
                )
                |> List.foldBack (fun t state ->
                    match fst t with
                    | Some s -> (s :: (fst state), snd t @ snd state) 
                    | None -> (fst state, snd t @ snd state) ) <| ([],[])
        }

    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        match combination with
        | TypeCombination.Union union ->
            let! (types, summary) = interpretTypes union |> InnerInterpretConfig.wrapFuncWithPrn<TopLevelFsStatement list * Summary>

            if types.Length = 0 then return None, summary
            elif types.Length = 1 then return types.Head |> Some, summary
            else
                return
                    {
                        Kind = FsStatementKind.Type FsStatementType.Union
                        Scope = Inherit
                        Open = "Fable.Core" :: (types |> List.map (fun ns -> ns.Open) |> List.concat)
                        CodeItems =
                            [
                                vmType $"U{types.Length}"
                                vmPrn "<"
                                yield! 
                                    types
                                    |> List.map (fun s ->
                                        s |> FsStatementV2.codeItems
                                    )
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
            let! (types, summary) = 
                interpretTypes dtsTypeList |> InnerInterpretConfig.unwrapFuncWithPrn

            let! fieldStartWith = config.FieldStartWithCodeItems

            let fields =
                types
                |> List.filter (fun s -> FsStatementV2.isAnonymousType s || FsStatementV2.isFieldListType s)
                |> List.map (fun s ->
                    s.NestedStatements
                    |> List.map (fun ns ->
                        {ns with 
                            CodeItems = [
                                tab (tabLevel + 1)
                                yield! fieldStartWith (ns |> FsStatementV2.identifier |> Option.get) 
                            ]
                            NestedStatements = ns.NestedStatements |> List.map FsStatementV2.addLineBreak
                        }
                    )
                )
                |> List.concat

            if not (fields |> List.isEmpty) then
                return
                    {
                        Kind = FsStatementKind.Type FsStatementType.Composition
                        Scope = Inherit
                        Open = (types |> List.map (fun ns -> ns.Open) |> List.concat)
                        CodeItems = []
                        NestedStatements = fields
                        PostCodeItems = []
                        Summary = summary
                        Hidden = false
                    } |> Some
                    , summary
            else return None, summary
    }


let interpretFuncSignature fl typeDefinition kind codeItemPrependix codeItemAppendix =
    interpreter {
        let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask

        let! returnTypeInterpretation = interpretTypeDefinition typeDefinition |> InnerInterpretConfig.wrapFuncWithPrn<TopLevelFsStatement option * Summary>
        let (returnType, summary) =
            match returnTypeInterpretation with
            | Some s, summary ->
                s, summary
            | None, summary -> 
                FsStatementV2.objType, summary

        let! (parameters, summary2) = 
            if fl |> List.isEmpty then Interpreter.retn ([FsStatementV2.unitType], [])
            else 
                fl 
                |> List.map interpretFnParameter
                |> Interpreter.sequence
                |> Interpreter.map (fun fl' ->
                    fl'
                    |> List.foldBack (fun t state -> (fst t :: fst state, snd t @ snd state)) <| ([], [])
                )
                |> InnerInterpretConfig.wrapFuncWithPrn

        return 
            {
                Kind = kind
                Scope = Inherit
                Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
                CodeItems = codeItemPrependix
        
                NestedStatements = [
                    {
                        Kind = kind
                        Scope = Inherit
                        Open = []
                        CodeItems =
                            [
                                yield!
                                    parameters
                                    |> List.map (fun ns -> ns |> FsStatementV2.codeItems)
                                    |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
                                vmPrn " -> "
                                yield! returnType |> FsStatementV2.codeItems
                            ]
        
                        NestedStatements = []
                        PostCodeItems = []
                        Summary = []
                        Hidden = false
                    }
                ]
                PostCodeItems = codeItemAppendix
                Summary = []
                Hidden = false
            }, summary @ summary2
    }


let interpretNamedFuncSignature fl typeDefinition kind codeItemPrependix codeItemAppendix =
    interpreter {
        let! returnTypeInterpretation = interpretTypeDefinition typeDefinition |> InnerInterpretConfig.wrapFuncWithPrn<TopLevelFsStatement option * Summary>
        let (returnType, summary) =
            match returnTypeInterpretation with
            | Some s, summary ->
                s, summary
            | None, summary -> 
                FsStatementV2.objType, summary

        let! (parameters, summary2) = 
            if fl |> List.isEmpty then Interpreter.retn ([FsStatementV2.unitType], [])
            else 
                fl 
                |> List.map interpretFnParameter
                |> Interpreter.sequence
                |> Interpreter.map (fun fl' ->
                    fl'
                    |> List.foldBack (fun t state -> (fst t :: fst state, snd t @ snd state)) <| ([], [])
                )
                |> InnerInterpretConfig.wrapFuncWithPrn

        return 
            {
                Kind = kind
                Scope = Inherit
                Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
                CodeItems = codeItemPrependix
                NestedStatements = [
                    {
                        Kind = kind
                        Scope = Inherit
                        Open = []
                        CodeItems =
                            [
                                yield!
                                    parameters
                                    |> List.map (fun ns -> 
                                    (
                                        ns |> FsStatementV2.identifier
                                        |> Option.map (fun id ->
                                            [
                                                vmIdentifier id
                                                vmPrn ": "
                                            ]
                                        )
                                        |> Option.defaultValue []
                                    )
                                     @ (ns |> FsStatementV2.codeItems))
                                    |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
                                vmPrn " -> "
                                yield! returnType |> FsStatementV2.codeItems
                            ]
        
                        NestedStatements = []
                        PostCodeItems = []
                        Summary = []
                        Hidden = false
                    }
                ]
                PostCodeItems = codeItemAppendix
                Summary = summary @ summary2
                Hidden = false
            }, summary @ summary2
    }


let rec interpretFnParameter (field, typeDefinition) : Interpreter<InnerInterpretConfig, (TopLevelFsStatement * Summary)> =
    
    let fsStatement identifier nested =
        {
            Kind = FsStatementKind.Parameter identifier
            Scope = Inherit
            Open = []
            CodeItems = []
            NestedStatements = [nested]
            PostCodeItems = []
            Summary = []
            Hidden = false
        }

    interpreter {
        let! (config: InnerInterpretConfig, _) = Interpreter.ask

        match field with
        | Field.Required identifier ->
            let! typeDef = interpretTypeDefinition typeDefinition
            match typeDef with
            | Some s, summary ->
                return fsStatement identifier s, summary
            | None, summary -> 
                return fsStatement identifier FsStatementV2.objType, summary

        | Field.Optional identifier ->
            let! typeDef = interpretTypeDefinition typeDefinition
            match typeDef with
            | Some s, summary ->
                return fsStatement identifier {s with PostCodeItems = s.PostCodeItems @ [vmType " option"]}, summary
            | None, summary -> 
                return fsStatement identifier {FsStatementV2.objType with PostCodeItems = FsStatementV2.objType.PostCodeItems @ [vmType " option"]}, summary

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let! signature = config.InterpretFuncSignature fl typeDefinition (FsStatementKind.Parameter identifier) [vmPrn "("] [vmPrn ")"]
            return signature
    }


let rec interpretField (field, typeDefinition) =
    let fsStatement identifier nested =
        interpreter {
            let! (config: InnerInterpretConfig, tabLevel) = Interpreter.ask
            let! fieldCodeItems = config.FieldStartWithCodeItems

            return {
                Kind = FsStatementKind.Field identifier
                Scope = Inherit
                Open = []
                CodeItems = fieldCodeItems identifier
                NestedStatements = [nested]
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
                let! topFsStatement = fsStatement identifier s
                return
                    (topFsStatement, snd td)
            | None -> 
                let! topFsStatement = fsStatement identifier FsStatementV2.objType
                return (topFsStatement, snd td)

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            let! fieldCodeItems = config.FieldStartWithCodeItems
            let! funcSignature = (config.InterpretFuncSignature fl typeDefinition (FsStatementKind.Field identifier) (fieldCodeItems identifier) [] |> withNamelessFuncSignature)
            return funcSignature
    }


let interpretInterface (interfaceDefinition: InterfaceDefinition) =
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
                    NestedStatements = nestedStatements |> List.map fst |> List.map FsStatementV2.addLineBreak
                    PostCodeItems = postCodeItems'
                    Summary = nestedStatements |> List.map snd |> List.concat
                    Hidden = false
                }

        | InterfaceDefinition.Extends (identifier, extendedType, fieldList) -> 
            let! (extendedTypeInterpretation, extendedSummary) =
                interpretDTsType extendedType

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
                        |> Option.map (fun s ->
                            nestedStatements |> List.map fst |> List.append [s]
                        )
                        |> Option.defaultWith (fun () -> nestedStatements |> List.map fst |> List.map FsStatementV2.addLineBreak) 
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
        | TypeAlias.Generic (identifier, _, combination)
        | TypeAlias.Plain (identifier, combination) ->
            let! comb = interpretTypeCombination combination
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
                    NestedStatements =
                        match fst comb with
                        | Some s -> [s; ]
                        | None -> []
                    PostCodeItems = postCodeItems'
                    Summary = snd comb
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
            let! (fsStatementOpt, summary) = interpretTypeDefinition td |> withDisabledTypeSearching
            return {
                Kind = FsStatementKind.Const identifier
                Scope = Scope.Inherit
                Open = []
                CodeItems = []
                NestedStatements = fsStatementOpt |> Option.map List.singleton |> Option.defaultValue []
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
                        NestedStatements = s.NestedStatements |> List.map FsStatementV2.addLineBreak
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
                    NestedStatements = fieldFsStatements
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
            let! signature = config.InterpretFuncSignature fl retType (FsStatementKind.Type FsStatementType.Func) [] []
            return
                {
                    Kind = FsStatementKind.LetImport identifier
                    Scope = Scope.Module (ModuleScope.Main)
                    Open = ["Fable.Core"]
                    CodeItems = [
                        vmPrn "[<"; vmText "Import"; vmPrn $"(\"{Identifier.value identifier}\", "; vmText "from="; vmPrn $"@\"{config.LibRelativePath.Value}\")>]"; vmEndLineNull
                        vmKeyword "let "; vmIdentifier (identifier |> Identifier.map Helpers.uncapitalizeFirstLetter); vmPrn " : "
                    ]
                    NestedStatements = [fst signature]
                    PostCodeItems = [
                        vmPrn " = "
                        vmText "jsNative"
                        vmEndLineNull
                    ]
                    Summary = snd signature
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


let withNamedFuncSignature interpreter =
    interpreter
    |> Interpreter.withEnv (fun (config, tabLevel) ->
        { config with InterpretFuncSignature = interpretNamedFuncSignature }, tabLevel
    )


let withNamelessFuncSignature interpreter =
    interpreter
    |> Interpreter.withEnv (fun (config, tabLevel) ->
        { config with InterpretFuncSignature = interpretFuncSignature }, tabLevel
    )


let withDisabledTypeSearching interpreter =
    interpreter
    |> Interpreter.withEnv (fun (config, tabLevel) ->
        { config with IsTypeSearchEnabled = false }, tabLevel
    )

let withEnableTypeSearching interpreter =
    interpreter
    |> Interpreter.withEnv (fun (config, tabLevel) ->
        { config with IsTypeSearchEnabled = true }, tabLevel
    )

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


let anonymous =
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
              InterpretFuncSignature = interpretNamedFuncSignature
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
              InterpretFuncSignature = interpretFuncSignature
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