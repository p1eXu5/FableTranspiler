namespace FableTranspiler.VmAdapters

open FableTranspiler.Infrastruture
open FableTranspiler.Parsers.Types
open FableTranspiler.Parsers.Dsl
open System.Windows
open System.Windows.Documents
open System
open System.Linq
open System.Collections.Generic
open System.IO

type FileTreeViewModel =
    {
        FileName: string
        StatementsResult: StatementsResult
        Modules: FileTreeViewModel list

    }


module internal FileTree =
    
    let rec toFileTreeVm (moduleTree: ModuleTree) =

        match moduleTree with
        | Leaf leaf ->
            {
                FileName = Path.GetFileName(leaf.Path)
                StatementsResult = leaf
                Modules = []
            }
        | Branch (root, branch) ->
            let modules =
                branch
                |> List.map toFileTreeVm

            {
                FileName = Path.GetFileName(root.Path)
                StatementsResult = root
                Modules = modules
            }
                    
