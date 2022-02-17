namespace FableTranspiler.Ports

module Persistence =
    
    open System.IO
    open System.Threading.Tasks
    open FableTranspiler.Parsers.Types
    open FableTranspiler.SimpleTypes

    type ReadFileAsync = FullPath -> Task<TextReader>

    type StatementStore =
        {
            ContainsKey : FullPath -> bool
            TryAdd : FullPath -> Result<StatementList, string> -> bool
            GetOrAdd: FullPath -> (unit -> Result<StatementList, string>) -> Result<StatementList, string>
            TryGetStatementList: FullPath -> Result<StatementList, string> option
            TryGetStatement : FullPath -> Identifier -> Statement option 
        }
