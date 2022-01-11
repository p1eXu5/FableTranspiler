namespace FableTranspiler.VmAdapters

open FableTranspiler.Infrastruture
open System
open System.Linq
open System.IO
open System.Collections.Generic
open FableTranspiler.VmAdapters.DocumentSegmentViewModel

type FileTreeViewModel
    (
        FileName: string,
        StatementsResult: StatementsResult,
        Modules: FileTreeViewModel list
    ) =

        static let dict = Dictionary<string, Dictionary<string, FsDocumentSegmentListViewModel>>()

        let interpretError (err: string) =
            (err.Split(Environment.NewLine)
            |> Array.toList
            |> List.map (fun s ->
                [
                    vmText s
                    vmEndLineNull
                ]
            )
            |> List.concat
            |> List.append 
            <| [
                DocumentSegmentViewModel.vmEndDocument
            ]).ToList()
            

        let dtsDocumentSegmentVmCollection =
            lazy (
                match StatementsResult.Statements with
                | Ok l -> DtsDocumentInterpreter.toDocumentSegmentVmList l
                | Error err -> interpretError err
            )

        let fsDocumentSegmentVmCollection =
            lazy (
                match StatementsResult.Statements with
                | Ok l -> FsDocumentInterpreter.toDocumentSegmentVmList None FileName dict l
                | Error err -> interpretError err
            )

        do
            if dict.ContainsKey(FileName) |> not then
                dict[FileName] <- Dictionary<string, FsDocumentSegmentListViewModel>()
                

        member val DtsDocumentSegmentVmCollection = dtsDocumentSegmentVmCollection.Value
        member val FsDocumentSegmentVmCollection = fsDocumentSegmentVmCollection.Value
        member val FileName = FileName
        member val Modules = Modules


module internal FileTree =
    
    let rec toFileTreeVm (moduleTree: ModuleTree) =

        match moduleTree with
        | Leaf leaf ->
            FileTreeViewModel (
                FileName = Path.GetFileName(leaf.Path),
                StatementsResult = leaf,
                Modules = []
            )
        | Branch (root, branch) ->
            let modules =
                branch
                |> List.map toFileTreeVm

            FileTreeViewModel (
                FileName = Path.GetFileName(root.Path),
                StatementsResult = root,
                Modules = modules
            )
                    
