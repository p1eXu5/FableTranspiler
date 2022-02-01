[<RequireQualifiedAccess>]
module internal FableTranspiler.VmAdapters.FsInterpreter.Fable

open FableTranspiler.Parsers.Types
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters.FsInterpreter.Common


let private interpretField (statements: string -> FsStatement option) (field: Field * TypeDefinition) : CodeItem list =
    match field with
    | ((Field.Required (Identifier name)), td) -> 
        [
            vmKeyword "abstract "; vmText name; vmPrn " : "
            match interpretTypeDefinition statements td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm.Construct())
            vmEndLineNull
        ]

    | (Field.Optional (Identifier name), td) ->
        [
            vmKeyword "abstract "; vmText name; vmPrn " : "
            match interpretTypeDefinition statements td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm.Construct())
            vmType " option"
            vmEndLineNull
        ]

    | (Field.FuncReq ((Identifier name), fl), td) ->
        [
            vmKeyword "abstract "; vmTextS name
            yield! interpretFieldFnParameters statements fl
            vmPrn " : "
            match interpretTypeDefinition statements td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm.Construct())
            vmEndLineNull
        ]

    | (Field.FuncOpt ((Identifier name), fl), td) ->
        [
            vmKeyword "abstract "; vmText name; 
            vmPrn " : ("
            yield! interpretFieldFnParameters statements fl
            vmPrn " -> "
            match interpretTypeDefinition statements td with
            | Choice1Of2 l -> yield! l
            | Choice2Of2 vm -> yield! (vm.Construct())
            vmPrn ") "
            vmType "option"
            vmEndLineNull
        ]


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