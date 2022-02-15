namespace FableTranspiler.Components

open System
open System.IO
open FableTranspiler.SimpleTypes
open FableTranspiler.AppTypes
open FableTranspiler.Interpreters
open FableTranspiler.Interpreters.FsInterpreter


/// Contains `Dts` and `Fs` statement generators
[<ReferenceEquality>]
type ModuleTree =
    {
        ModulePath : ModulePath
        RootKey : LibLocation
        /// fileName :: parent
        Key : string list
        IsSelected : bool
        FileName : string
        SubModules : ModuleTree list
        DtsDocumentVm : Lazy<Choice< DtsStatementViewModel list, CodeItem list>>
        FsDocumentVm : Interpreter<InterpretConfig, Choice< FsStatementViewModel list, InterpretationError>>
    }



module internal ModuleTree =

    open FableTranspiler.Interpreters.FsInterpreter.InterpreterBuilder

    let initialInterpreters =
        {
            InterpretPlainFableInterface = FableTranspiler.Interpreters.FsInterpreter.Fable.interpretPlainFableInterface
        }


    let build libLocation parsingResultTree =
        
        let interpretError (err: string) =
            (err.Split(Environment.NewLine)
            |> Array.toList
            |> List.map (fun s ->
                [
                    vmText s
                    vmEndLineNull
                ]
            )
            |> List.concat)
    
        let rec build libLocation parentKey (parsingResultTree: FileParsingResultTree) =

            let moduleTree (parentKey, parsingResult: ParsingResult, subModules) : ModuleTree =
                let fileName = Path.GetFileName(parsingResult.ModulePath |> ModulePath.Value)
                {
                    ModulePath = parsingResult.ModulePath
                    RootKey = libLocation
                    Key = fileName :: parentKey
                    IsSelected = false
                    FileName = fileName
                    SubModules = subModules
                    DtsDocumentVm =
                        lazy (
                            match parsingResult.Statements with
                            | Ok statements -> 
                                DtsInterpreter.interpret statements 
                                |> List.map DtsStatementViewModel.create
                                |> Choice1Of2
                            | Error err -> interpretError err |> Choice2Of2
                        )
                    FsDocumentVm =
                        interpreter {
                            match parsingResult.Statements with
                            | Ok statements ->
                                    let! fsStatements = 
                                        FsInterpreter.Facade.interpret 
                                            None // namespace
                                            parsingResult.ModulePath 
                                            statements
                                    return
                                        fsStatements
                                        |> List.map FsStatementViewModel.create
                                        |> Choice1Of2
                            | Error err -> 
                                return
                                    interpretError err |> Choice2Of2
                        }
                }

            match parsingResultTree with
            | Leaf leaf ->
                moduleTree (
                    parentKey,
                    leaf,
                    []
                )
            | Branch (root, branch) ->
                let vm = 
                    moduleTree (
                        parentKey,
                        root,
                        []
                    )

                let subModules =
                    branch
                    |> List.map (fun tree -> 
                        build libLocation vm.Key tree
                    )
                    
                {vm with SubModules = subModules}

        build libLocation [] parsingResultTree
