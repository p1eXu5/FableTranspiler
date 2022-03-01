namespace FableTranspiler.Ports

module Persistence =
    
    open System.IO
    open System.Threading.Tasks
    open FableTranspiler.Parsers.Types
    open FableTranspiler.SimpleTypes

    type ReadFileAsync = FullPath -> Task<TextReader>

    type StatementStore<'TStatement> =
        {
            ContainsKey : FullPath -> bool
            TryAdd : FullPath -> Result<'TStatement list, string> -> bool
            AddOrUpdate : FullPath -> Result<'TStatement list, string> -> Result<'TStatement list, string>
            GetOrAdd: FullPath -> (unit -> Result<'TStatement list, string>) -> Result<'TStatement list, string>
            TryGetStatementList: FullPath -> Result<'TStatement list, string> option
            TryGetStatement : FullPath -> Identifier -> 'TStatement option 
        }
