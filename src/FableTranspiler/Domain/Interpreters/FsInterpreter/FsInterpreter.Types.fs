namespace FableTranspiler.Interpreters.FsInterpreter

open System
open FableTranspiler.Parsers.Types
open FableTranspiler.SimpleTypes
open FableTranspiler.Interpreters
open Microsoft.Extensions.Logging
open System.Text





[<RequireQualifiedAccess>]
type FsStatement =
    | Nameless of CodeItem list
    | Named of Identifier * CodeItem list
    | Link of Identifier * FsStatement
    | Let of Identifier * Display: CodeItem list * Signature: CodeItem list
    | Typed of Identifier * Display: CodeItem list * Body: CodeItem list


type FsStatementReader = 
    {
        CurrentModulePath: ModulePath
        ImportedModules: Map<Identifier, ModulePath>
        Get: Identifier list -> FsStatement option
    }

type FsStatementStore =
    {
        Get: ModulePath -> Identifier -> FsStatement option
        Add: ModulePath -> FsStatement -> unit

        /// Initializes new FsStatementReader with set current module path
        /// and no imported modules.
        InitReader: ModulePath -> FsStatementReader

        ImportAll: Identifier -> ModulePath -> FsStatementReader -> FsStatementReader
    }


type InterpretationError = CodeItem list


type FsCodeStyle =
    | Universal
    | Fable
    | React
    | Feliz


[<ReferenceEquality>]
type FsStatementDto =
    {
        Index: int
        DtsStatement: FableTranspiler.Parsers.Types.Statement option
        StyledFsStatements: StyledFsStatement list
    }
and
    StyledFsStatement =
        {
            FsCodeStyle: FsCodeStyle
            FsStatement: FsStatement
        }


type Interpreters =
    {
        InterpretPlainFableInterface: Identifier -> FieldList -> TabLevel -> Interpreter<FsStatementReader, (CodeItem list * CodeItem list)>
    }

type InterpretConfig =
    {
        Store: FsStatementStore
        Interpreters: Interpreters
        LoggerFactory: ILoggerFactory
        TryFindModule: ModulePath -> StatementList option
    }


[<RequireQualifiedAccess>]
module internal FsStatement =

    let name = function
        | FsStatement.Nameless _ -> None
        | FsStatement.Named (n, _) -> n |> Some
        | FsStatement.Link (n, _) -> n |> Some
        | FsStatement.Let (n, _, _) -> n |> Some
        | FsStatement.Typed (n, _, _) -> n |> Some

    let rec codeItems = function
        | FsStatement.Nameless l -> l
        | FsStatement.Named (_, l) -> l
        | FsStatement.Link (_, l) -> l |> codeItems
        | FsStatement.Let (_, l, _) -> l
        | FsStatement.Typed (_, l, _) -> l

    let rec body = function
        | FsStatement.Nameless _ -> None
        | FsStatement.Named (_, l) -> l |> Some
        | FsStatement.Link (_, l) -> l |> body
        | FsStatement.Let (_, _, signature) -> signature |> Some
        | FsStatement.Typed (_, _, body) -> body |> Some

    let rec insertAtEnd segment = function
        | FsStatement.Nameless l -> l @ [segment] |> FsStatement.Nameless
        | FsStatement.Named (name, l) -> (name, l @ [segment]) |> FsStatement.Named
        | FsStatement.Link (name, l) -> (name, insertAtEnd segment l) |> FsStatement.Link
        | FsStatement.Let (name, l, constructor) -> (name, l @ [segment], constructor) |> FsStatement.Let
        | FsStatement.Typed (name, l, constructor) -> (name, l @ [segment], constructor) |> FsStatement.Typed




type FsStatement with
    member this.CodeItems() = FsStatement.codeItems this
    member this.Name() = FsStatement.name this
    member this.StringContent() =
        this.CodeItems()
        |> List.map (fun ci ->
            match ci.Tag with
            | Tag.NoContent -> ""
            | Tag.EndOfLine -> "\n"
            | _ -> ci.Content
        )
        |> fun l -> String.Join("", l)
    member this.Construct() =
        match FsStatement.body this with
        | Some l -> l
        | None -> []


[<RequireQualifiedAccess>]
module internal FsStatementDto =

    let create dtsStatement ind codeStyle fsStatement =
        {
            Index = ind
            DtsStatement = dtsStatement
            StyledFsStatements = 
                [{
                    FsCodeStyle = codeStyle
                    FsStatement = fsStatement
                }]
        }


type FsStatementDto with
    member this.Content() =
        raise (NotImplementedException())
        //this.StyledFsStatements[this.SelectedFsStatement]
        //    .FsStatement
        //    |> FsStatement.segments

    member this.FsCodeStyle() =
        raise (NotImplementedException())
        //this.StyledFsStatements[this.SelectedFsStatement]
        //    .FsCodeStyle

