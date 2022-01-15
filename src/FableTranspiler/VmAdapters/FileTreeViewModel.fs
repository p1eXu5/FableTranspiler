namespace FableTranspiler.VmAdapters

open FableTranspiler.Infrastruture
open System
open System.Linq
open System.IO
open System.Collections.Generic
open FableTranspiler.VmAdapters.DocumentSegmentViewModel


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

    let dict = Dictionary<string, Dictionary<string, FsDocumentSection>>()


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


    let create(key, fileName, isSelected, parsingResult, subModules) =
        if dict.ContainsKey(fileName) = false then
            dict[fileName] <- Dictionary<string, FsDocumentSection>()

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
                    | Ok l -> FsDocumentInterpreter.toDocumentSegmentVmList None fileName dict l |> Choice1Of2
                    | Error err -> interpretError err |> Choice2Of2
                )
        }

    
    let rec toFileTreeVm parentKey isSelected (moduleTree: ModuleTree) =

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
                |> List.map (toFileTreeVm key false)

            create (
                key,
                fileName,
                isSelected,
                root,
                modules
            )
                    
