namespace FableTranspiler.VmAdapters

open FableTranspiler.Infrastruture
open System
open System.Linq
open System.IO

type FileTreeViewModel
    (
        FileName: string,
        StatementsResult: StatementsResult,
        Modules: FileTreeViewModel list
    ) =
        let documentSegmentVmCollection =
            lazy (
                match StatementsResult.Statements with
                | Ok l -> DocumentSegment.toDocumentSegmentVmList l
                | Error err ->
                    (err.Split(Environment.NewLine)
                    |> Array.toList
                    |> List.map (fun s ->
                        [
                            { Tag = Tag.Text; Content = s }
                            { Tag = Tag.EndOfLine; Content = null }
                        ]
                    )
                    |> List.concat
                    |> List.append 
                    <| [
                        { Tag = Tag.EndOfDocument; Content = null }
                    ]).ToList()
                    
            )

        member val DocumentSegmentVmCollection = documentSegmentVmCollection.Value
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
                    
