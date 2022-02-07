namespace FableTranspiler.Components

open System
open System.IO
open FableTranspiler.SimpleTypes
open FableTranspiler.AppTypes
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters
open FableTranspiler.VmAdapters.FsInterpreter.Types
open FableTranspiler.VmAdapters.FsInterpreter.Interpreter


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
        FsDocumentVm : Lazy<Choice< FsStatementViewModel list, CodeItem list>>
    }



module internal ModuleTree =

    let initialInterpreters =
        {
            InterpretPlainFableInterface = FableTranspiler.VmAdapters.FsInterpreter.Fable.interpretPlainFableInterface
        }


    
    let rec init store (loggerFactory: Microsoft.Extensions.Logging.ILoggerFactory) libLocation parentKey isSelected (moduleTree: FileParsingResultTree) =
        
        let config : Config = 
            {
                Store = store
                Interpreters = initialInterpreters
                LoggerFactory = loggerFactory
            }

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

        let create (parentKey, parsingResult: ParsingResult, subModules) : ModuleTree =
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
                        | Ok l -> 
                            DtsInterpreter.interpret l 
                            |> List.map DtsStatementViewModel.create
                            |> Choice1Of2
                        | Error err -> interpretError err |> Choice2Of2
                    )
                FsDocumentVm =
                    lazy (
                        match parsingResult.Statements with
                        | Ok l ->
                            try
                                // interpretError "need to fix (ModuleTree.fs)" |> Choice2Of2
                                FsInterpreter.Facade.interpret None parsingResult.ModulePath l
                                |> run config
                                |> List.map FsStatementViewModel.create
                                |> Choice1Of2
                            with
                            | ex -> interpretError ex.Message |> Choice2Of2
                        | Error err -> interpretError err |> Choice2Of2
                    )
            }

        match moduleTree with
        | Leaf leaf ->
            create (
                parentKey,
                leaf,
                []
            )
        | Branch (root, branch) ->
            let vm = 
                create (
                    parentKey,
                    root,
                    []
                )

            let subModules =
                branch
                |> List.map (init store loggerFactory libLocation vm.Key false)

            {vm with SubModules = subModules}


    //let produceDocuments moduleTree store =
    //    let interpretError (err: string) =
    //        (err.Split(Environment.NewLine)
    //        |> Array.toList
    //        |> List.map (fun s ->
    //            [
    //                vmText s
    //                vmEndLineNull
    //            ]
    //        )
    //        |> List.concat)

    //    {
    //        moduleTree with
    //            DtsDocumentVm =
    //                match moduleTree.ParsingResult.Statements with
    //                | Ok l -> 
    //                    DtsInterpreter.interpret l 
    //                    |> List.map DtsStatementViewModel.create
    //                    |> Choice1Of2
    //                | Error err -> interpretError err |> Choice2Of2
    //                |> Some

    //            FsDocumentVm =
    //                match moduleTree.ParsingResult.Statements with
    //                | Ok l -> 
    //                    FsInterpreter.Facade.interpret None moduleTree.FileName store initialInterpreters l 
    //                    |> List.map FsStatementViewModel.create
    //                    |> Choice1Of2
    //                | Error err -> interpretError err |> Choice2Of2
    //                |> Some
    //    }


    let bindings () =
        [
            
        ]
                    
