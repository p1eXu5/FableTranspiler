[<RequireQualifiedAccess>]
module internal FableTranspiler.VmAdapters.FsInterpreter.Fable

open FableTranspiler.Parsers.Types
open FableTranspiler.VmAdapters.Types
open FableTranspiler.VmAdapters.FsInterpreter.Common
open FableTranspiler.VmAdapters.FsInterpreter.InterpreterBuilder
open FableTranspiler.SimpleTypes


let private interpretField (field: Field * TypeDefinition) =
    interpreter {
        let! (fsStatementReader: FsStatementReader) = Interpreter.ask

        match field with
        | ((Field.Required (Identifier name)), td) -> 
            let! typeInterpretation = interpretTypeDefinition td
            return
                [
                    vmKeyword "abstract "; vmText name; vmPrn " : "
                    match typeInterpretation with
                    | Choice1Of2 l -> yield! l
                    | Choice2Of2 vm -> yield! (vm.Construct())
                    vmEndLineNull
                ]

        | (Field.Optional (Identifier name), td) ->
            let! typeInterpretation = interpretTypeDefinition td
            return
                [
                    vmKeyword "abstract "; vmText name; vmPrn " : "
                    match typeInterpretation with
                    | Choice1Of2 l -> yield! l
                    | Choice2Of2 vm -> yield! (vm.Construct())
                    vmType " option"
                    vmEndLineNull
                ]

        | (Field.FuncReq ((Identifier name), fl), td) ->
            let! fnParametersInterpretation = interpretFieldFnParameters fl
            let! typeInterpretation = interpretTypeDefinition td
            return
                [
                    vmKeyword "abstract "; vmTextS name
                    yield! fnParametersInterpretation
                    vmPrn " : "
                    match typeInterpretation with
                    | Choice1Of2 l -> yield! l
                    | Choice2Of2 vm -> yield! (vm.Construct())
                    vmEndLineNull
                ]

        | (Field.FuncOpt ((Identifier name), fl), td) ->
            let! fnParametersInterpretation = interpretFieldFnParameters fl
            let! typeInterpretation = interpretTypeDefinition td
            return
                [
                    vmKeyword "abstract "; vmText name; 
                    vmPrn " : ("
                    yield! fnParametersInterpretation
                    vmPrn " -> "
                    match typeInterpretation with
                    | Choice1Of2 l -> yield! l
                    | Choice2Of2 vm -> yield! (vm.Construct())
                    vmPrn ") "
                    vmType "option"
                    vmEndLineNull
                ]
    }



let internal interpretPlainFableInterface identifier fieldList (tabLevel: TabLevel) =
    interpreter {
        let! (fsStatementReader: FsStatementReader) = Interpreter.ask
        return
            [
                tab tabLevel
                vmKeyword "type "
                vmTextS (identifier |> Identifier.Value)
                vmText "="
                vmEndLineNull
                yield!
                    fieldList 
                    |> List.map (fun t -> (tab (tabLevel + 1)) :: (interpretField t |> Interpreter.run fsStatementReader))
                    |> List.concat
                vmEndLineNull
            ],
            [ vmType (identifier |> Identifier.Value) ]
    }

let interpretators =
    {
        InterpretPlainFableInterface = interpretPlainFableInterface
    }