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



    let rec apply 
        ( createNode:'state -> FullPath -> 'node,
          addSubmodules: 'node -> 'node list -> 'node, 
          createErrorNode,
          state,
          getState: 'node -> 'state )
          tree =
              match tree with
              | Node (fp, importing) -> 
                  let node' = fp |> createNode state
                  let state' = getState node'
                  importing 
                  |> List.map (apply (createNode, addSubmodules, createErrorNode, state', getState))
                  |> addSubmodules node'
              | ErrorNode (fp, err) -> ((fp |> createNode state), err) ||> createErrorNode


    let rec allNodes fullPathTree =
        fullPathTree
        :: (
            fullPathTree 
            |> importingList
            |> List.map allNodes
            |> List.concat
        )