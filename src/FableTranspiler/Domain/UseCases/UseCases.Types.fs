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

    let tryFind targetUri startFromLevel graph =
        let rec goToLevel level xgraph =
            if level > 0 then
                goToLevel (level - 1) (xgraph |> List.map importingList |> List.concat)
            else
                xgraph

        let rec tryFind' xgraph =
            if (xgraph |> List.length) = 0 then None
            else
                match xgraph |> List.tryFind (fun node -> (node |> fullPath) = targetUri) with
                | Some g -> g |> Some
                | None  ->
                    tryFind' (xgraph |> List.map importingList |> List.concat)
            
        if startFromLevel > 0 then
            goToLevel startFromLevel [graph]
            |> tryFind'
        else tryFind' [graph]


