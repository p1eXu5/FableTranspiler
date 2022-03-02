module internal rec FableTranspiler.Interpreters.FsInterpreter.React

open FableTranspiler.SimpleTypes
open FableTranspiler.Helpers
open FableTranspiler.Parsers.Types
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter
open FableTranspiler.Interpreters.FsInterpreter.Common
open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder
open System
open FableTranspiler.Interpreters.FsInterpreter.FsStatementV2

type Summary = CodeItem list



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
                        return fsStatement (FsStatementType.FieldList identifiers) (statement.NestedStatements |> List.map codeItems |> List.concat) |> Some, []
                    | None -> 
                        return fsStatement (FsStatementType.Unknown typeName) [vmType typeName] |> Some, []
            | _ -> 
                match config.TryGetStatement identifiers with
                | Some statement ->
                    return fsStatement (FsStatementType.FieldList identifiers) (statement.NestedStatements |> List.map codeItems |> List.concat) |> Some, []
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
        | DTsType.Void -> return unitType |> Some, []

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
                                    |> List.map (fun ns -> ns |> codeItems)
                                    |> List.reduce (fun t1 t2 -> t1 @ [vmPrn ", "] @ t2)
                                vmPrn ">"
                            ]
            
                        NestedStatements = []
                        PostCodeItems = []
                        Summary = []
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
                    } |> Some
                    , summary
            else return None, summary
    }


let interpretTypeDefinition (tdef: TypeDefinition) : Interpreter<InnerInterpretConfig, (FsStatementV2 option * Summary)> =
    interpreter {
        match tdef with
        | TypeDefinition.Single tn -> return! interpretDTsType tn
        | TypeDefinition.Combination combination -> return! interpretTypeCombination combination 
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
                            |> List.map (fun ns -> ns |> codeItems)
                            |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
                        vmPrn " -> "
                        yield! returnType.CodeItems
                        vmPrn ")"
                        yield! codeItemAppendix
                    ]
        
                NestedStatements = []
                PostCodeItems = []
                Summary = []
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
                PostCodeItems = []
                Summary = []
            }

        match field with
        | Field.Required identifier
        | Field.Optional identifier ->
            let! td = interpretTypeDefinition typeDefinition
            match fst td with
            | Some s ->
                return
                    fsStatement identifier [s |> addLineBreak], snd td
            | None -> return fsStatement identifier [FsStatementV2.objType |> addLineBreak], snd td

        | Field.FuncReq (identifier, fl)
        | Field.FuncOpt (identifier, fl) ->
            return! interpretFuncSignature fl typeDefinition (FsStatmentKind.Field identifier) (fieldCodeItems identifier) [vmEndLineNull]

            //let! returnType = interpretTypeDefinition typeDefinition
            //let parameters = 
            //    if fl |> List.isEmpty then [FsStatementV2.unitType]
            //    else fl |> List.map (runFnParameterInterpretation (config, tabLevel))
            //return
            //    {
            //        Identifier = FsStatmentKind.Field identifier
            //        Scope = Inherit
            //        Open = returnType.Open @ (parameters |> List.map (fun ns -> ns.Open) |> List.concat)
            //        CodeItems =
            //            [
            //                yield! fieldCodeItems identifier
            //                vmPrn "("
            //                yield!
            //                    parameters
            //                    |> List.map (fun ns -> ns |> codeItems)
            //                    |> List.reduce (fun t1 t2 -> t1 @ [vmPrn " -> "] @ t2)
            //                vmPrn " -> "
            //                yield! returnType.CodeItems
            //                vmPrn ")"
            //                vmEndLineNull
            //            ]
                
            //        NestedStatements = []
            //        Summary = []
            //    }
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
                    PostCodeItems = htmlPropsInheritance (tabLevel + 1)
                    Summary = nestedStatements |> List.map snd |> List.concat
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
                    PostCodeItems = htmlPropsInheritance (tabLevel + 1)
                    Summary = snd comb
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
            }
    }

let internal strategy =
    {
        InterpretInterface = interpretInterface
        InterpretTypeAlias = interpretTypeAlias
        InterpretReactComponent = interpretReactComponent
    }