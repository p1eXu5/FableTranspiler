namespace FableTranspiler.VmAdapters

open System
open System.IO
open FableTranspiler.AppTypes
open FableTranspiler.VmAdapters.CodeItemViewModel


type FileTreeViewModel =
    {
        /// fileName :: parent
        Key: string list
        IsSelected: bool
        FileName: string
        SubModules: FileTreeViewModel list
        DtsDocumentVm: Lazy< Choice< DtsStatementViewModel list, CodeItemViewModel list>>
        FsDocumentVm: Lazy< Choice< FsStatementViewModel list, CodeItemViewModel list>>
    }



module internal FileTree =

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


    let initialInterpreters =
        {
            InterpretPlainFableInterface = FableTranspiler.VmAdapters.FsInterpreter.Fable.interpretPlainFableInterface
        }


    let create (key, fileName, isSelected, parsingResult, subModules, store) =
        {
            Key = key
            IsSelected = isSelected
            FileName = fileName
            SubModules = subModules
            DtsDocumentVm =
                lazy (
                    match parsingResult.Statements with
                    | Ok l -> DtsDocumentInterpreter.toDocumentSegmentVmList l |> Choice1Of2
                    | Error err -> interpretError err |> Choice2Of2
                )
            FsDocumentVm =
                lazy (
                    match parsingResult.Statements with
                    | Ok l -> FsInterpreter.Facade.interpret None fileName store initialInterpreters l |> Choice1Of2
                    | Error err -> interpretError err |> Choice2Of2
                )
        }

    
    let rec toFileTreeVm store parentKey isSelected (moduleTree: ModuleTree) =

        match moduleTree with
        | Leaf leaf ->
            let fileName = Path.GetFileName(leaf.Path)
            create (
                fileName :: parentKey,
                fileName,
                isSelected,
                leaf,
                [],
                store
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
                modules,
                store
            )
                    
