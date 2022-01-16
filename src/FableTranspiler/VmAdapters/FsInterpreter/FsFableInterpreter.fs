module internal FableTranspiler.VmAdapters.FsInterpreter.Fable

open FableTranspiler.VmAdapters
open FableTranspiler.VmAdapters.FsInterpreter.Common
open FableTranspiler.Parsers.Types


let private interpretField (statements: string -> FsStatement option) (field: Field * TypeDefinition) : CodeItemViewModel list =
    match field with
    | ((Field.Required (Identifier name)), td) -> 
        [
            vmKeyword "abstract "
            vmText name
            vmPrn " : "
            match interpretTypeDefinition statements td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm |> FsStatement.construct)
            vmEndLineNull
        ]

    | (Field.FuncReq ((Identifier name), fl), td) ->
        [
            vmKeyword "abstract "
            vmTextS name
            yield! interpretFnParameters statements fl
            vmPrn " : "
            match interpretTypeDefinition statements td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm |> FsStatement.construct)
            vmEndLineNull
        ]

    | _ -> failwith "Not implemented"



let interpretPlainFableInterface statements tabLevel name fieldList =
    [
        tab tabLevel
        vmKeyword "type "
        vmTextS name
        vmText "="
        vmEndLineNull
        yield!
            fieldList 
            |> List.map (fun t -> (tab (tabLevel + 1)) :: interpretField statements t)
            |> List.concat
        vmEndLineNull
    ],
    [ vmType name ]