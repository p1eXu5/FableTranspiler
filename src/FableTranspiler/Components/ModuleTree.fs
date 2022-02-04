namespace FableTranspiler.Components

open System
open System.IO
open FableTranspiler.AppTypes
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters
open FableTranspiler.Parsers.Types


[<ReferenceEquality>]
type ModuleTree =
    {
        RootKey: ModulePath
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

        let create (key, fileName, isSelected, parsingResult, subModules) =
            {
                RootKey = modulePath
                Key = key
                IsSelected = isSelected
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
                            FsInterpreter.Facade.interpret None fileName store initialInterpreters l
                            |> List.map FsStatementViewModel.create
                            |> Choice1Of2
                        | Error err -> interpretError err |> Choice2Of2
                    )
            }

        match moduleTree with
        | Leaf leaf ->
            let fileName = Path.GetFileName(leaf.Path)
            create (
                fileName :: parentKey,
                fileName,
                isSelected,
                leaf,
                []
            )
        | Branch (root, branch) ->
            let fileName = Path.GetFileName(root.Path)
            let key = fileName :: parentKey

            let modules =
                branch
                |> List.map (init store modulePath key false)

            create (
                key,
                fileName,
                isSelected,
                root,
                modules
            )


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
                    
