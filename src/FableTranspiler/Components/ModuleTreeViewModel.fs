namespace FableTranspiler.Components

open System
open System.IO
open FableTranspiler.AppTypes
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters


[<ReferenceEquality>]
type ModuleTreeViewModel =
    {
        ParsingResult: ParsingResult
        /// fileName :: parent
        Key: string list
        IsSelected: bool
        FileName: string
        SubModules: ModuleTreeViewModel list
        DtsDocumentVm: Choice< DtsStatementViewModel list, CodeItem list> option
        FsDocumentVm: Choice< FsStatementViewModel list, CodeItem list> option
    }



module internal ModuleTreeViewModel =

    let initialInterpreters =
        {
            InterpretPlainFableInterface = FableTranspiler.VmAdapters.FsInterpreter.Fable.interpretPlainFableInterface
        }



    
    let rec toFileTreeVm store parentKey isSelected (moduleTree: FileParsingResultTree) =
        
        let create (key, fileName, isSelected, parsingResult, subModules) =
            {
                ParsingResult = parsingResult
                Key = key
                IsSelected = isSelected
                FileName = fileName
                SubModules = subModules
                DtsDocumentVm = None
                FsDocumentVm = None
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
                |> List.map (toFileTreeVm store key false)

            create (
                key,
                fileName,
                isSelected,
                root,
                modules
            )


    let produceDocuments moduleTree store =
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

        {
            moduleTree with
                DtsDocumentVm =
                    match moduleTree.ParsingResult.Statements with
                    | Ok l -> 
                        DtsInterpreter.interpret l 
                        |> List.map DtsStatementViewModel.create
                        |> Choice1Of2
                    | Error err -> interpretError err |> Choice2Of2
                    |> Some

                FsDocumentVm =
                    match moduleTree.ParsingResult.Statements with
                    | Ok l -> 
                        FsInterpreter.Facade.interpret None moduleTree.FileName store initialInterpreters l 
                        |> List.map FsStatementViewModel.create
                        |> Choice1Of2
                    | Error err -> interpretError err |> Choice2Of2
                    |> Some
        }


    let bindings () =
        [
            
        ]
                    
