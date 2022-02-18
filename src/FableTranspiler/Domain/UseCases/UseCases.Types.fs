namespace FableTranspiler.Domain.UseCases

open FableTranspiler.SimpleTypes

type ParseFileUseCase = FullPath -> FullPathTree
and 
    FullPathTree =
        | Node of Node: FullPath * Importing: FullPathTree list
        | ErrorNode of Node: FullPath * Error: string


module FullPathTree =

    let fullPath = function
        | FullPathTree.Node (fp, _)
        | FullPathTree.ErrorNode (fp, _) -> fp

    let (|ImportingUriList|_|) = function
        | FullPathTree.Node (_, l) -> l |> List.map (fullPath) |> Some
        | _ -> None

    let (|ImportedToUriList|_|) = function
    | FullPathTree.Node (_, l) -> l |> List.map (fullPath) |> Some
    | _ -> None

    let importingList = function
    | FullPathTree.Node (_, l) -> l
    | _ -> []

    let tryFind targetUri startFromLevel tree =
        let rec goToLevel level xtree =
            if level > 0 then
                goToLevel (level - 1) (xtree |> List.map importingList |> List.concat)
            else
                xtree

        let rec tryFind' xtree =
            if (xtree |> List.length) = 0 then None
            else
                match xtree |> List.tryFind (fun node -> (node |> fullPath) = targetUri) with
                | Some g -> g |> Some
                | None  ->
                    tryFind' (xtree |> List.map importingList |> List.concat)
            
        if startFromLevel > 0 then
            goToLevel startFromLevel [tree]
            |> tryFind'
        else tryFind' [tree]


    let rec apply (ffp, fn, fer, state, getState) tree =
        match tree with
        | Node (fp, l) -> 
            let node' = fp |> ffp state
            let state' = getState node'
            let importing' = l |> List.map (apply (ffp, fn, fer, state', getState))
            (node', importing') ||> fn
        | ErrorNode (fp, err) -> ((fp |> ffp state), err) ||> fer


    let rec allNodes fullPathTree =
        fullPathTree
        :: (
            fullPathTree 
            |> importingList
            |> List.map allNodes
            |> List.concat
        )