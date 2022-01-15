module FableTranspiler.Tests.FParsecTests

open NUnit.Framework
open FsUnit
open FParsec


let ws    = spaces
let ws1   = spaces1
let str s = pstring s

let inline writeLine s = TestContext.WriteLine(sprintf "%A" s)

let inputsIter p inputs =
    inputs
    |> List.iter (fun i ->
        let res = run p i
        writeLine res
    )



[<Test>]
let ``nextCharSatisfiesNot test`` () =
    let tagOpenBegin tag =
        str ("<" + tag)
        >>? notFollowedBy (satisfy isLetter) // make sure tag name is complete
        <?> "<" + tag + "> tag"

    let tagOpen tag = tagOpenBegin tag >>. str ">" >>? nextCharSatisfiesNot isLetter

    TestContext.WriteLine("<div sdfsfskfj")
    TestContext.WriteLine( run (tagOpenBegin "div") "<div sdfsfskfj")
    TestContext.WriteLine( run (tagOpen "div") "<div>")


[<Test>]
let ``alteration operator test`` () =
    let str s = pstring s
    let ab = str "a" .>>. str "b"
    let ac = str "a" .>>. str "c"

    let ab' = str "ab"
    let ac' = str "ac"

    run (ab <|> ac) "ac"
    |> writeLine

    run (ab' <|> ac') "ac"
    |> writeLine


[<Test>]
let ``pipe operator test`` () =
    let ch : Parser<_,_> = choice [
        (str "()")
        (str "c")
    ]

    let pipe2' p2 f p1 = pipe2 p1 p2 f
    let chainl' op p = chainl p op
        
    let p = 
        pipe2 (str "a") (str "b") (+) 
        |> pipe2' ch (+)

    run (p) "abc"
    |> writeLine


[<Test>]
let ``choice test`` () =
    let p = choice [
        str "a" >>. str "d" //.>>. str "b"
        str "a"
        str "b" //.>>. str "c"
        str "c" //.>>. str "d"
    ]

    let inputs = [
        "a"  // fail, choosen first altyeration after first parser changes stateTag
        "aa" // fail, choosen first altyeration after first parser changes stateTag
        "b"
        "c"
        "ad"
    ]

    inputsIter p inputs

    writeLine "=================="

    let p2 = (str "a" .>>.? str "d" |>> (fun t -> t ||> (+))) <|> str "a"
    let inputs2 = [
        "ad"
        "a"
        "aa"
    ]
    
    inputsIter p2 inputs2

    ()


[<Test>]
let ``backtracking operator test`` () =

    let p = str "a" .>>? str "b" .>>? str "c" .>> eof
    let input = "ab"

    let res = run p input
    writeLine res

    let pp = p <|> str "ab"
    let res = run pp input
    writeLine res

    ()


[<Test>]
let ``backtracking operator test 2`` () =

    let p = pchar 'a' .>>? pchar 'b' .>>? pchar 'c'
    let input = "ac"

    let res = run p input // failed
    writeLine res

    ()


[<Test>]
let ``backtracking operator test 3`` () =

    let p1 = pchar 'a' .>> pchar 'b' .>>? pchar 'c'
    let p2 = pchar 'a' .>> pchar 'b' .>>? pchar 'd'
    let p = choice [
        p1
        p2
    ]

    let input = "abd"

    let res = run p input // success
    writeLine res

    ()

[<Test>]
let ``backtracking operator test 4`` () =

    let p1 = pchar 'a' .>> pchar 'f' .>>? pchar 'c'
    let p2 = pchar 'a' .>> pchar 'b' .>>? pchar 'd'
    let p = choice [
        p1
        p2
    ]

    let input = "abd"

    let res = run p input // failed
    writeLine res

    ()


// Function or Identifier

type Foo =
    | Empty
    | Identifier of string
    | Function of string * Foo

[<Test>]
let ``identifier or method test`` () =

    let input1 = "foo"

    let inputs = [
        input1
        "foo()"
        "foo( bar )"
        "foo( bar() )"
    ]

    let elements, elementsR = createParserForwardedToRef()
    

    let p1 = 
        FableTranspiler.Parsers.Parser.preIdentifier
        .>> ws1
        .>> notFollowedBy (str "(")
        |>> Identifier


    writeLine "==============="
    let r = run p1 input1
    writeLine r
    writeLine "==============="


    let p2 = 
        FableTranspiler.Parsers.Parser.preIdentifier
        .>> str "()"
        |>> (fun (i) -> Foo.Function (i, Foo.Empty))

    let p3 = 
        FableTranspiler.Parsers.Parser.preIdentifier 
        .>> str "(" 
        .>> ws 
        .>>. elements 
        .>> ws 
        .>> str ")" 
        |>> (fun (i, expr) -> Foo.Function (i, expr))


    elementsR := choice [
        attempt(p1)
        attempt(p2)
        attempt(p3)
    ]

    inputsIter elements inputs

    ()