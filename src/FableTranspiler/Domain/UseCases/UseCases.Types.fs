namespace FableTranspiler.Domain.UseCases

open FableTranspiler.SimpleTypes

type ParseFileUseCase = FullPath -> UriGraph
and 
    UriGraph =
        | Root of Node: FullPath * Importing: UriGraph list
        | Node of Node: FullPath * Importing: UriGraph list * ImportedTo: UriGraph list
        | ErrorNode of Node: FullPath * Error: string


module UriGraph =
    let root = function
        | UriGraph.Root (uri, importing) -> (uri, importing) |> Some
        | _ -> None

    let uri = function
        | UriGraph.Root (uri, _)
        | UriGraph.Node (uri, _, _)
        | UriGraph.ErrorNode (uri, _) -> uri

    let (|ImportingUriList|_|) = function
        | UriGraph.Root (_, l) 
        | UriGraph.Node (_, l, _) -> l |> List.map (uri) |> Some
        | _ -> None

    let (|ImportedToUriList|_|) = function
    | UriGraph.Node (_, _, l) -> l |> List.map (uri) |> Some
    | _ -> None

    let importingList = function
    | UriGraph.Root (_, l) 
    | UriGraph.Node (_, l, _) -> l
    | _ -> []

    let tryFind targetUri startFromLevel graph =
        let rec tryFindRun level xgraph =
            if level > 0 then
                tryFindRun (level - 1) (xgraph |> List.map importingList |> List.concat)
            else
                xgraph |> List.tryFind (fun node -> (node |> uri) = targetUri)

        tryFindRun startFromLevel [graph]


