module rec FableTranspiler.VmAdapters.FsInterpreter.Facade

open FableTranspiler.Parsers.Types
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters.FsInterpreter.Common
open System
open FableTranspiler.SimpleTypes
open System.IO
open Types
open FableTranspiler.VmAdapters.FsInterpreter.InterpreterBuilder


let importAttribute name source =
    [
        vmPrn "[<"
        vmType "Import"
        vmPrn "("
        vmText $"\"{name}\", "
        vmKeyword "from"
        vmText $"=\"{source}\""
        vmPrn ")>]"
    ]





let private interpretStructure (structure: StructureStatement) tabLevel =
    let tabbedImportAttribute name jsModule tabLevel =
        [
            tab tabLevel
            yield! importAttribute name jsModule
            vmEndLineNull
        ]

    interpreter {

        let! (
            config: {|
                ImportingJsModule: string; 
                FsStatementReader: FsStatementReader; 
                Interpreters: Interpreters
            |}
        ) = Interpreter.ask

        match structure with
        //| FunctionDefinition (FunctionDefinition.Plain (identifier, parameters, returnType)) ->
        //    return
        //        (
        //            identifier,
        //            [
        //                yield! tabbedImportAttribute identifier config.ImportingJsModule tabLevel
        //                tab tabLevel
        //                yield! interpretFn statements "let" identifier parameters returnType
        //                vmEndLineNull
        //                vmEndLineNull
        //            ],
        //            (fun () -> interpretFnType statements parameters returnType )
        //        )
        //        |> FsStatement.Let

        | InterfaceDefinition (InterfaceDefinition.Plain (identifier, fl)) ->
            let! (present, construct) = 
                config.Interpreters.InterpretPlainFableInterface identifier fl tabLevel
                |> Interpreter.withEnv (fun config -> config.FsStatementReader)

            return
                (
                    identifier,
                    present,
                    construct
                )
                |> FsStatement.Interface

        //| ConstDefinition (DeclareConst (identifier, tdef)) ->
        //    match interpretTypeDefinition statements tdef with
        //    | Choice1Of2 l -> return FsStatement.Named (identifier, l)
        //    | Choice2Of2 vm -> return FsStatement.Link (identifier, vm)

        | _ -> 
            return
                [ 
                    vmText ($"{structure} interpretation is not implemented")
                    vmEndLineNull
                ]
                |> FsStatement.Nameless
    }




let internal toDocumentSegmentViewModelList (fsList: FsStatement list) : CodeItem list =
    fsList
    |> List.map FsStatement.codeItems
    |> List.concat


let rec private _interpret statements tabLevel ind (result: FsStatementDto list) =
    interpreter {

        let! (
            config: {|
                Store : FsStatementStore
                Interpreters : Interpreters
                FsStatementReader : FsStatementReader
                ModulePath: ModulePath
                ImportingJsModule: string
            |}
        ) = Interpreter.ask
    
    /// append generated view models to the result and invokes interpret
        let continueInterpret tail vm =
            _interpret tail tabLevel (ind + 1) (vm :: result)

        match statements with
        | statement :: tail ->

            let createDto vm = 
                let fsCodeStyle =
                    match vm with
                    | FsStatement.Interface _ -> FsCodeStyle.Fable
                    | FsStatement.Typed _ -> FsCodeStyle.Fable
                    | _ -> FsCodeStyle.Universal
                FsStatementDto.create (statement |> Some) ind fsCodeStyle vm

            match statement with
            | Statement.Export (ExportStatement.Structure structure) ->
                let! (vm: FsStatement) = 
                    interpretStructure structure tabLevel
                    |> Interpreter.withEnv (fun config ->
                        {|
                            ImportingJsModule = config.ImportingJsModule
                            FsStatementReader = config.FsStatementReader
                            Interpreters = config.Interpreters
                        |}
                    )
                do
                    match vm |> FsStatement.name with
                    | Some n -> config.Store.Add config.ModulePath n vm
                    | None -> ()

                return! continueInterpret tail (vm |> createDto)

            //| Statement.Structure structure ->
            //    let vm = 
            //        interpretStructure interpreters tabLevel jsModuleName (store.Get modulePath) structure

            //    do
            //        match vm |> FsStatement.name with
            //        | Some n -> store.Add modulePath n vm
            //        | None -> ()

            //    return! _interpret tabLevel tail (ind + 1) result

            | Statement.Export (ExportStatement.OutDefault identifier) ->
                let vm = 
                    match config.FsStatementReader identifier with
                    | Some fsStatement -> createDto fsStatement
                    | None -> 
                        FsStatement.Nameless [
                            vmText $"%A{statement} is not parsed"
                            vmEndLineNull
                        ] |> createDto

                return! continueInterpret tail vm

            | _ -> return! _interpret tail tabLevel ind result

        | [] -> return result |> List.rev
    }
    


let internal interpret ns modulePath statements : Interpreter<Config, FsStatementDto list> =

    interpreter {
        let fileName = Path.GetFileNameWithoutExtension( modulePath |> ModulePath.Value )
        let jsModuleName = String( fileName |> Seq.takeWhile ((=) '.' >> not) |> Seq.toArray )


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
            (
                Identifier fsModuleName,
                [
                    vmKeyword "module "
                    vmText fsModuleName
                    vmEndLine null
                    vmEndLine null
                ] 
            )
            |> FsStatement.Named
            |> FsStatementDto.create None -1 FsCodeStyle.Universal
            |> List.singleton

        return! 
            _interpret statements (TabLevel 0) 0 initialResult
            |> Interpreter.withEnv (fun config -> 
                {| 
                    config with 
                        FsStatementReader = (fun _ -> None)
                        ModulePath = modulePath
                        ImportingJsModule = jsModuleName
                |})
    }
