namespace FableTranspiler.Components

open System
open System.IO
open FableTranspiler.SimpleTypes
open FableTranspiler.AppTypes
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters
open FableTranspiler.VmAdapters.FsInterpreter.Types


[<ReferenceEquality>]
type ModuleTree =
    {
        RootKey: LibLocation
        /// fileName :: parent
        Key: string list
        IsSelected: bool
        FileName: string
        SubModules: ModuleTree list
        DtsDocumentVm: Lazy<Choice< DtsStatementViewModel list, CodeItem list>>
        FsDocumentVm: Lazy<Choice< FsStatementViewModel list, CodeItem list>>
    }



module internal ModuleTree =

    let initialInterpreters =
        {
            InterpretPlainFableInterface = FableTranspiler.VmAdapters.FsInterpreter.Fable.interpretPlainFableInterface
        }



    
    let rec init store modulePath parentKey isSelected (moduleTree: FileParsingResultTree) =
        
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

        let create (parentKey, parsingResult, subModules) =
            let fileName = Path.GetFileName(parsingResult.Path |> ModulePath.Value)
            {
                RootKey = modulePath
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
                                interpretError "need to fix (ModuleTree.fs)" |> Choice2Of2
                                //FsInterpreter.Facade.interpret None modulePath store initialInterpreters l
                                //|> List.map FsStatementViewModel.create
                                //|> Choice1Of2
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
                |> List.map (init store modulePath vm.Key false)

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
                    
