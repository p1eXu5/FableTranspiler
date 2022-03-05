module rec FableTranspiler.Interpreters.DtsInterpreter

open FableTranspiler.Parsers.Types
open System.Linq
open FableTranspiler.Interpreters
open FableTranspiler.SimpleTypes


[<ReferenceEquality>]
type DtsStatement =
    {
        StatementIndex: int
        Presentation: CodeItem list
    }

[<RequireQualifiedAccess>]
module DtsStatement =

    let create ind codeItems =
        {
            StatementIndex = ind
            Presentation = codeItems
        }



type EntityOrder =
    | Single
    | First
    | Last
    | Middle

let private modulePath = function
    | NodeModule (ModulePath path) -> vmText $"'%s{path}'"
    | Relative (ModulePath path) -> vmText $"'%s{path}'"


let private insertWithOrder entityOrder posfix content =
    match entityOrder with
    | Single -> sprintf "{ %s }%s" content posfix
    | First -> sprintf "{ %s, " content
    | Last -> sprintf "%s }%s" content posfix
    | Middle -> sprintf "%s, " content


let private insertAsWithOrder entityOrder nameTag name alias postfix =
    match entityOrder with
    | First ->
        [
            vmPrn "{ "
            vm nameTag name
            vmKeyword " as "
            vmType alias
            vmPrn ", "
        ]
    | Last ->
        [
            vm nameTag name
            vmKeyword " as "
            vmType alias
            vmPrn $" }}{postfix}"
        ]
    | Middle ->
        [
            vm nameTag name
            vmKeyword " as "
            vmType alias
            vmPrn ", "
        ]
    | Single ->
        [
            vmPrn "{ "
            vm nameTag name
            vmKeyword " as "
            vmType alias
            vmPrn $" }}{postfix}"
        ]


let private importEntity postfix entity entityOrder = 
    match entity with
    | No -> [vmNo]
    | ImportEntity.Named (Identifier identifier) -> [ identifier |> insertWithOrder entityOrder postfix |> vmText ]
    | Aliased (Identifier name, Identifier alias) -> insertAsWithOrder entityOrder Tag.Text name alias postfix
    | All -> [vmPrn "* "]
    | AllAliased (Identifier alias) -> 
        [
            vmPrn "* "
            vmKeyword "as "
            vmTypeS alias
        ]


let private exportEntity postfix entity entityOrder = 
    match entity with
    | ExportEntity.Named (Identifier identifier) -> [identifier |> insertWithOrder entityOrder postfix |> vmText]
    | DefaultAliased (Identifier alias) -> insertAsWithOrder entityOrder Tag.Modifier "default" alias postfix


let rec private buildImportExportEntities builder l res =
    match l with
    | head::[] when res |> List.isEmpty -> 
        builder head Single

    | head::[] when res |> List.isEmpty |> not -> 
        res @ builder head Last

    | head::tail when res |> List.isEmpty ->
        let res' = res @ builder head First
        buildImportExportEntities builder tail res'

    | head::tail when res |> List.isEmpty |> not ->
        let res' = res @ builder head Middle
        buildImportExportEntities builder tail res'

    | _ -> []
                

let private constructType l : CodeItem list =
    l
    |> List.map (fun t -> [vmType (Identifier.value(t))])
    |> List.reduce (fun t1 t2 -> 
        [
            yield! t1
            vmText "."
            yield! t2
        ]
    )


let private constructSingleType tn =
    constructCombination "" [tn] []


let rec private constructObjectLiteral (fields: (Field * TypeDefinition) list) (endLine: bool * string) =
    fields
    |> List.mapi (fun ind l ->
        let (fld, tdef) = l
        [
            if fst endLine then
                yield vmText "    "

            match fld with
            | Required (Identifier i) ->
                yield vmText i
                yield vmPrn ": "
            | Optional (Identifier i) ->
                yield vmText i
                yield vmPrn "?: "
            | FuncOpt (Identifier i, fl) ->
                yield vmText i
                yield vmPrn "?("
                yield! constructObjectLiteral fl (false, ", ")
                yield vmPrn "): "
            | FuncReq (Identifier i, fl) ->
                yield vmText i
                yield vmPrn "("
                yield! constructObjectLiteral fl (false, ", ")
                yield vmPrn "): "

            yield! constructTypeDefinition tdef
            //match tdef with
            //| TypeDefinition.Single tn -> 
            //    yield! constructSingleType tn
            //| TypeDefinition.Combination comb ->
            //    match comb with
            //    | Union l ->
            //        yield! constructCombination " | " l []
            //    | Composition l ->
            //        yield!  constructCombination " & " l [] 



            if fst endLine then
                yield vmEndLine <| snd endLine
            elif ind < (fields.Length - 1) then
                yield vmText <| snd endLine
        ]
    )
    |> List.concat


let constructTypeDefinition tdef =
    [
        match tdef with
        | TypeDefinition.Single tn -> 
            yield! constructSingleType tn
        | TypeDefinition.Combination comb ->
            match comb with
            | Union l ->
                yield! constructCombination " | " l []
            | Composition l ->
                yield!  constructCombination " & " l [] 
    ]


let rec private constructCombination sep combination res =
    match combination with
    | [] -> 
        res
        |> List.rev
        |> List.reduce (fun t1 t2 -> 
            [
                yield! t1
                vmPrn sep
                yield! t2
            ]
        )
    | head :: tail ->
        match head with
        | DTsType.InlineObject fl ->
            [
                vmPrn "{"
                yield! constructObjectLiteral fl (false, ", ")
                vmPrn "}"
            ]
                

        | DTsType.Plain p -> 
            constructType p
            |> (fun r -> constructCombination sep tail (r :: res))
                    
        | DTsType.Generic (p, i) ->
            let main = constructType p
            let inner = constructCombination ", " i []
            let l =
                [
                    yield! main
                    vmText "<"
                    yield! inner
                    vmText ">"
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Undefined -> 
            let l =
                [
                    vmType "undefined"
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Void -> 
            let l =
                [
                    vmType "void"
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Any -> 
            let l =
                [
                    vmType "any"
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Func (fl, tdef) ->
            let l =
                [
                    vmPrn "(("
                    yield! constructObjectLiteral fl (false, ", ")
                    vmPrn ") => "

                    yield! constructTypeDefinition tdef
                    vmPrn ")"
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Typeof is ->
            let l =
                [
                    vmKeyword "typeof "
                    yield! constructType is
                ]
            constructCombination sep tail (l :: res)

        | DTsType.Array t ->
            let l = 
                [
                    yield! constructSingleType t
                    vmPrn "[]"
                ]
            constructCombination sep tail (l :: res)



let constructTypeParams typeParams =
    typeParams
    |> List.mapi (fun ind (Identifier i) ->
        [
            vmType i
            if ind < (typeParams.Length - 1) then
                vmText ", "
        ]
    )
    |> List.concat


let rec private interpretStructure structure : CodeItem list =
    match structure with
    | TypeAlias (TypeAlias.Plain (Identifier identifier, combination)) ->
        [
            yield vmKeyword "type "
            yield vmType identifier
            yield vmText " = "
            match combination with
            | Union l ->
                yield! constructCombination " | " l []
            | Composition l ->
                yield!  constructCombination " & " l []
            yield vmEndLine ";"
        ]

    | TypeAlias (TypeAlias.Generic (Identifier identifier, typeParams, combination)) ->
        [
            yield vmKeyword "type "
            yield vmType identifier
            yield vmPrn "<"
            yield! constructTypeParams typeParams 
            yield vmText " = "
            match combination with
            | Union l ->
                yield! constructCombination " | " l []
            | Composition l ->
                yield!  constructCombination " & " l []
            yield vmEndLine ";"
        ]

    | ClassDefinition (ExtendsEmpty (Identifier idetifier, tn)) -> 
        [
            yield vmKeyword "class "
            yield vmType idetifier
            yield vmKeyword " extends "

            yield! constructSingleType tn // construct combination from single operand

            yield vmPrn "{}"
            yield vmEndLineNull
        ]

    | InterfaceDefinition (Extends (Identifier idetifier, tn, lit)) ->
        [
            yield vmKeyword "interface "
            yield vmType idetifier
            yield vmKeyword " extends "
                    
            yield! constructSingleType tn  // construct combination from single operand

            yield vmPrn "{"
            yield vmEndLineNull
                    
            yield! constructObjectLiteral lit (true, ", ")
                    
            yield vmPrn "}"
            yield vmEndLineNull
        ]

    | InterfaceDefinition (InterfaceDefinition.Plain (Identifier idetifier, fl)) ->
        [
            yield vmKeyword "interface "
            yield vmType idetifier

            yield vmPrn "{"
            yield vmEndLineNull
                    
            yield! constructObjectLiteral fl (true, ", ")
                    
            yield vmPrn "}"
            yield vmEndLineNull
        ]

    | FunctionDefinition (FunctionDefinition.Plain ((Identifier i), fl, tdef)) ->
        [
            yield vmKeyword "function "
            yield vmType i

            yield vmPrn "("
                    
            yield! constructObjectLiteral fl (false, ", ")
                    
            yield vmPrn ")"
            yield vmText ": "

            yield! constructTypeDefinition tdef
            yield vmEndLine ";"
        ]

    | FunctionDefinition (FunctionDefinition.Generic ((Identifier i), il, fl, tdef)) ->
        [
            yield vmKeyword "function "
            yield vmType i
            yield vmPrn "<"
            yield! constructTypeParams il
            yield vmPrn ">"

            yield vmPrn "("
                    
            yield! constructObjectLiteral fl (false, ", ")
                    
            yield vmPrn ")"
            yield vmText ": "

            yield! constructTypeDefinition tdef
            yield vmEndLine ";"
        ]

    | FunctionDefinition (FunctionDefinition.GenericNameless (il, fl, tdef)) ->
        [
            yield vmKeyword "function"
            yield vmPrn "<"
            yield! constructTypeParams il
            yield vmPrn ">"

            yield vmPrn "("
                    
            yield! constructObjectLiteral fl (false, ", ")
                    
            yield vmPrn ")"
            yield vmText ": "

            yield! constructTypeDefinition tdef
            yield vmEndLine ";"
        ]

    | StructureStatement.ConstDefinition (ConstDefinition.DeclareConst ((Identifier i), tdef)) ->
        [
            yield vmKeyword "declare const "
            yield vmText i
            yield vmText ": "
            yield! constructTypeDefinition tdef
            yield vmEndLine ";"
        ]

    | StructureStatement.ConstDefinition (ConstDefinition.Const ((Identifier i), tdef)) ->
        [
            yield vmKeyword "const "
            yield vmText i
            yield vmText ": "
            yield! constructTypeDefinition tdef
            yield vmEndLine ";"
        ]


let interpret statements =

    let rec interpret statements ind result lastTag : DtsStatement list =

        /// append generated view models to the result and invokes interpret
        let continueInterpret tail xvm =
            interpret tail (ind + 1) (xvm :: result)

        match statements with
        | statement :: tail ->

            let createVm = DtsStatement.create ind

            match statement with
            | Statement.Import (entities, ``module``) ->
                let s =
                    if lastTag <> "import"  then
                        [
                            vmEndLineNull
                            vmKeyword "import "
                        ]
                    else
                        [ vmKeyword "import " ]

                let vm =
                    [
                        yield! s
                        yield! (buildImportExportEntities (importEntity " ") entities [])
                        yield vmKeyword "from "
                        yield ``module`` |> modulePath
                        yield vmEndLine ";"
                    ]
                    |> createVm

                continueInterpret tail vm "import"

            | Statement.Export (Transit (entities, ``module``)) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            vmEndLineNull
                            vmModifier "export "
                        ]
                    else
                        [ vmModifier "export " ]

                let vm =
                    [
                        yield! s
                        yield! (buildImportExportEntities (exportEntity " ") entities [])
                        yield vmKeyword "from "
                        yield ``module`` |> modulePath
                        yield vmEndLine ";"
                    ]
                    |> createVm

                continueInterpret tail vm "export"


            | Statement.Export (OutList entities) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            vmEndLineNull
                            vmModifier "export "
                        ]
                    else
                        [ vmModifier "export " ]

                let vm =
                    [
                        yield! s
                        yield! (buildImportExportEntities (exportEntity "") (entities |> List.map (Named)) [])
                        yield vmEndLine ";"
                    ]
                    |> createVm

                continueInterpret tail vm "export"


            | Statement.Export (OutDefault (Identifier i)) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            vmEndLineNull
                            vmModifier "export "
                        ]
                    else
                        [ vmModifier "export " ]

                let vm =
                    [
                        yield! s
                        yield vmModifier "default "
                        yield vmText i
                        yield vmEndLine ";"
                    ]
                    |> createVm

                continueInterpret tail vm "export"


            | Statement.Export (OutAssignment (Identifier identifier)) ->
                let s =
                    if lastTag <> "export"  then
                        [
                            vmEndLineNull
                            vmModifier "export"
                        ]
                    else
                        [
                            vmModifier "export"
                        ]

                let vm =
                    [
                        yield! s
                        yield vmText " = "
                        yield vmType identifier
                        yield vmEndLine ";"
                    ]
                    |> createVm

                continueInterpret tail vm "export"


            | Statement.Export (ExportStatement.Structure structure) ->

                let (breakeLine, lastTag') = 
                    match structure with
                    | FunctionDefinition _ -> (lastTag = "export function" |> not, "export function")
                    | _ -> (true, "")

                let xvm = interpretStructure structure
                let vm =
                    [
                        // each time insert break line
                        if breakeLine then
                            yield vmEndLineNull
                            
                        yield vmModifier "export "
                            
                        yield! xvm
                    ]
                    |> createVm

                continueInterpret tail vm lastTag'

            | Statement.Export (ExportStatement.StructureDefault structure) ->
                
                    let (breakeLine, lastTag') = 
                        match structure with
                        | FunctionDefinition _ -> (lastTag = "export function" |> not, "export function")
                        | _ -> (true, "")
                
                    let xvm = interpretStructure structure
                    let vm =
                        [
                            // each time insert break line
                            if breakeLine then
                                yield vmEndLineNull
                                            
                            yield vmModifier "export "
                            yield vmKeyword "default "
                                            
                            yield! xvm
                        ]
                        |> createVm

                    continueInterpret tail vm lastTag'

            | Statement.Export (ExportStatement.Namespace ((Identifier i), statements')) ->
                let vm =
                    [
                        yield vmEndLineNull
                        yield vmModifier "export "
                        yield vmKeyword "namespace "
                        yield vmType i
                        yield vmPrn " {"
                        yield vmEndLineNull
                        yield! (interpret statements' (ind + 1) [] "" |> List.map (fun vm -> vm.Presentation) |> List.concat)
                        yield vmEndLineNull
                        yield vmPrn "}"
                        yield vmEndLineNull
                    ]
                    |> createVm

                continueInterpret tail vm ""


            | Statement.Structure structure ->
                let xvm = interpretStructure structure
                let vm =
                    [
                        yield vmEndLineNull
                        yield! xvm
                    ]
                    |> createVm

                continueInterpret tail vm ""

            | Statement.Comment comment ->
                let s =
                    if lastTag <> "comment"  then
                        [
                            yield vmEndLineNull
                            yield vmComment comment
                            yield vmEndLineNull
                        ]
                    else
                        [
                            yield vmComment comment
                            yield vmEndLineNull
                        ]

                continueInterpret tail (createVm s) "comment"

            | Statement.NamespaceDeclaration ((Identifier i), statements') ->
                let vm =
                    [
                        yield vmEndLineNull
                        yield vmKeyword "declare namespace "
                        yield vmType i
                        yield vmPrn " {"
                        yield vmEndLineNull
                        yield! (interpret statements' (ind + 1) [] "" |> List.map (fun vm -> vm.Presentation) |> List.concat)
                        yield vmEndLineNull
                        yield vmPrn "}"
                        yield vmEndLineNull
                    ]
                    |> createVm

                continueInterpret tail vm ""

            | _ -> interpret tail (ind + 1) result ""

        | [] -> 
            result 
            |> List.rev

    (interpret statements 0 [] "")
