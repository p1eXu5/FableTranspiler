module FableTranspiler.Parsers.Identifier

open FParsec
open Types
open Common


let stringLiteral = quote >>. manyCharsTill anyChar quote |>> (StringLiteral.Create >> Expression.StringLiteral)

let intOfFloatLiteral : Parser<_, unit>=
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
        if n.IsInteger then Expression.IntLiteral (int n.String)
        else Expression.FloatLiteral (float n.String)


let opp = OperatorPrecedenceParser<_,_,_>()
let expr = opp.ExpressionParser


let validIdentifierChars = "_"
let pipe2' p2 f p1 = pipe2 p1 p2 f

let inline (||>>) f (p1, p2) =
    p1
    >>= (fun p1' -> 
        p2 >>= (fun p2' -> f p1' p2')
    )

let preIdentifier =
    (letter <|> anyOf validIdentifierChars)
    .>>. manyChars (letter <|> digit <|> anyOf validIdentifierChars)
    |>> (fun (c, s) -> ((+) (string c) s))


let identifier = 
    preIdentifier
    .>> ws
    .>> notFollowedBy (pchar '(')
    |>> (Identifier.Create >> Expression.Identifier)


let ``parameterless function`` =
    preIdentifier
    .>> ws
    .>> str "()"
    |>> fun funcName -> Expression.Function (funcName, Expression.Empty)


let ``function`` =
    preIdentifier
    .>> pchar '(' 
    .>> ws 
    .>>. expr 
    .>> ws 
    .>> pchar ')' 
    |>> (fun (i, expr) -> Expression.Function (i, expr))



let objectLiteral =
    pchar '{'
    >>. ws
    >>. many (
        identifier
        .>> pchar ':'
        .>> ws
        .>>. stringLiteral
        .>> ws
        .>> pchar ','
        .>> ws
        |>> (fun (i, v) -> Expression.Binary (ExpressionKind.Typification, i, v))
    )
    .>> pchar '}'
    .>> ws
    |>> Expression.ObjectLiteral


opp.TermParser <- choice [
    intOfFloatLiteral 
    stringLiteral
    objectLiteral
    attempt(identifier)
    attempt(``parameterless function``)
    attempt(``function``)
]


let binary kind (l: Expression) (r: Expression) =
    Expression.Binary (kind, l, r)


opp.AddOperator <| InfixOperator("=", ws, 10, Associativity.Right, (binary ExpressionKind.Assignment) )
opp.AddOperator <| InfixOperator(".", ws, 20, Associativity.Right, (binary ExpressionKind.Dereferentiation) )
opp.AddOperator <| InfixOperator("as", ws, 30, Associativity.Right, (binary ExpressionKind.AsCast) )





let constStmt = skipString "const" >>? ws1 >>. expr .>> ws |>> Statement.Const

let stmt = choice [
    constStmt
]

let statement = 
    choice [
        Misc.declareConst
        Import.statement
        Export.statement
        Comment.statement
        Structures.statement |>> Statement.Structure
    ]

let query : Parser<Statements, _> = sepEndBy statement skipNewline

let queryFull = ws >>. query .>> spaces .>> eof

let document input =
    run queryFull input
    |> function
        | Success (ok,_,_) -> Result.Ok ok
        | Failure (err,_,_) -> Result.Error err