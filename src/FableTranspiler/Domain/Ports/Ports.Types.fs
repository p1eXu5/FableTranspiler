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
            TryAdd : FullPath -> StatementList -> bool
            TryGetValue : FullPath -> Identifier -> Statement option 
        }
