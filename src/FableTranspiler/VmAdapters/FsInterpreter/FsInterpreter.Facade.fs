module rec FableTranspiler.VmAdapters.FsInterpreter.Facade

open FableTranspiler.Parsers.Types
open FableTranspiler.VmAdapters
open FableTranspiler.VmAdapters.FsInterpreter.Common
open System.Linq
open System.Collections.Generic
open System


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



let tabbedImport tabLevel name fileName =
    [
        tab tabLevel
        yield! import name fileName
        vmEndLineNull
    ]


let private interpretStructure interpreters tabLevel fileName (statements: GetFsStatement) (structure: StructureStatement) : FsStatement =


    match structure with
    | FunctionDefinition (FunctionDefinition.Plain ((Identifier name), parameters, returnType)) ->
        (
            name,
            [
                yield! tabbedImport tabLevel name fileName
                tab tabLevel
                yield! interpretFn statements "let" name parameters returnType
                vmEndLineNull
                vmEndLineNull
            ],
            (fun () -> interpretFnType statements parameters returnType )
        )
        |> Let

    | InterfaceDefinition (InterfaceDefinition.Plain ((Identifier name), fl)) ->
        let present, construct = interpreters.InterpretPlainFableInterface statements tabLevel name fl
        (
            name,
            [
                yield! tabbedImport tabLevel name fileName
                yield! present
            ],
            construct
        )
        |> Typed

    | ConstDefinition (DeclareConst ((Identifier name), tdef)) ->
        match interpretTypeDefinition statements tdef with
        | Choice1Of2 l -> FsStatement.Named (name, l)
        | Choice2Of2 vm -> FsStatement.Link (name, vm)

    | _ -> failwith "Not implemented"



let toDocumentSegmentViewModelList (fsList: FsStatement list) : CodeItemViewModel list =
    fsList
    |> List.map FsStatement.segments
    |> List.concat


let interpret ns fileName store interpreters statements =

    let jsModuleName = String( fileName |> Seq.takeWhile ((=) '.' >> not) |> Seq.toArray )

    let rec interpret tabLevel statements (result: FsStatementViewModel list) : FsStatementViewModel list =

        /// append generated view models to the result and invokes interpret
        let continueInterpret tail vm =
            interpret tabLevel tail (vm :: result)

        match statements with
        | statement :: tail ->

            let createVm vm = 
                let fsCodeStyle =
                    match vm with
                    | Typed _ -> FsCodeStyle.Fable
                    | _ -> FsCodeStyle.Universal
                createFsVm (statement |> Some) fsCodeStyle vm

            match statement with
            | Statement.Export (ExportStatement.Structure structure) ->
                let vm = 
                    interpretStructure interpreters tabLevel jsModuleName (store.Get fileName) structure
                    

                store.Add fileName (vm |> FsStatement.name) vm
                continueInterpret tail (vm |> createVm)

            | Statement.Structure structure ->
                let vm = 
                    interpretStructure interpreters tabLevel jsModuleName (store.Get fileName) structure

                store.Add fileName (vm |> FsStatement.name) vm
                interpret tabLevel tail result

            | Statement.Export (ExportStatement.OutDefault (Identifier name)) ->
                continueInterpret tail (store.Get fileName name |> Option.get |> createVm)

            | _ -> interpret tabLevel tail result

        | [] -> result |> List.rev


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
        |> createFsVm None FsCodeStyle.Universal
        |> List.singleton

    (interpret 0 statements initialResult)