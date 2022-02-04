module rec FableTranspiler.VmAdapters.FsInterpreter.Facade

open FableTranspiler.Parsers.Types
open FableTranspiler.VmAdapters.Types
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
        |> FsStatement.Let

    | InterfaceDefinition (InterfaceDefinition.Plain ((Identifier name), fl)) ->
        let present, construct = interpreters.InterpretPlainFableInterface statements tabLevel name fl
        {
            Name = name
            Content = present
            Construct = construct
        }
        |> FsStatement.Interface

    | ConstDefinition (DeclareConst ((Identifier name), tdef)) ->
        match interpretTypeDefinition statements tdef with
        | Choice1Of2 l -> FsStatement.Named (name, l)
        | Choice2Of2 vm -> FsStatement.Link (name, vm)

    | _ -> 
        [ 
            vmText ($"{structure} interpretation is not implemented")
            vmEndLineNull
        ]
        |> FsStatement.Nameless



let internal toDocumentSegmentViewModelList (fsList: FsStatement list) : CodeItem list =
    fsList
    |> List.map FsStatement.segments
    |> List.concat


let internal interpret ns fileName store interpreters statements =

    let jsModuleName = String( fileName |> Seq.takeWhile ((=) '.' >> not) |> Seq.toArray )

    let rec interpret tabLevel statements ind (result: FsStatementDto list) : FsStatementDto list =

        /// append generated view models to the result and invokes interpret
        let continueInterpret tail vm =
            interpret tabLevel tail (ind + 1) (vm :: result)

        match statements with
        | statement :: tail ->

            let createDto vm = 
                let fsCodeStyle =
                    match vm with
                    | FsStatement.Typed _ -> FsCodeStyle.Fable
                    | _ -> FsCodeStyle.Universal
                FsStatementDto.create (statement |> Some) ind fsCodeStyle vm

            match statement with
            | Statement.Export (ExportStatement.Structure structure) ->
                let vm = 
                    interpretStructure interpreters tabLevel jsModuleName (store.Get fileName) structure
                    
                match vm |> FsStatement.name with
                | Some n -> store.Add fileName n vm
                | None -> ()

                continueInterpret tail (vm |> createDto)

            | Statement.Structure structure ->
                let vm = 
                    interpretStructure interpreters tabLevel jsModuleName (store.Get fileName) structure

                match vm |> FsStatement.name with
                | Some n -> store.Add fileName n vm
                | None -> ()
                interpret tabLevel tail (ind + 1) result

            | Statement.Export (ExportStatement.OutDefault (Identifier name)) ->
                let vm = 
                    match store.Get fileName name with
                    | Some fsStatement -> createDto fsStatement
                    | None -> 
                        FsStatement.Nameless [
                            vmText $"%A{statement} is not parsed"
                            vmEndLineNull
                        ] |> createDto

                continueInterpret tail vm

            | _ -> interpret tabLevel tail (ind + 1) result

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
        |> FsStatement.Nameless
        |> FsStatementDto.create None -1 FsCodeStyle.Universal
        |> List.singleton

    (interpret 0 statements 0 initialResult)