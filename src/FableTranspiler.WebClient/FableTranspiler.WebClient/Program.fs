module Program

open Fable.Core.JsInterop
open FSharp.Data
open Browser
open Fable.Core


importAll "./assets/App.css"


// =========
// functions
// =========

type IGreeter =
    abstract Greeter : name:string -> string


[<Import("greeter2", from="./${outDir}/../../../NodejsConsoleApp/lib/test.js")>]
let greeter2 : name: string -> string = jsNative

console.log(greeter2("John"))


[<Import("Greeter", from="./${outDir}/../../../NodejsConsoleApp/lib/test.js")>]
let greeter : name: string -> string = jsNative

console.log(greeter("John"))


[<ImportAll("./${outDir}/../../../NodejsConsoleApp/lib/test.js")>]
let greeterO : IGreeter = jsNative

console.log(greeterO.Greeter("John"))

let obj = createObj !![("a", 1)]
console.log(obj)




console.log("========= test interfaces ============")

// ==========
// interfaces
// ==========

type IFoo =
    abstract onDo : (unit -> unit) option
    abstract dutarion: U3<float, string, (float -> float)> option

//[<Import("execute", from="./${outDir}/../../../NodejsConsoleApp/lib/test-interface.js")>] // also valid
[<Import("execute", from="./${outDir}/../../../NodejsConsoleApp/lib/test.js")>]
let execute(foo: IFoo) : unit = jsNative

let foo : IFoo = 
    !!{|
        onDo = (fun () -> console.log("onDo")) |> Some
        duration = fun n -> n * 2.f // 13, "13", fun n -> n * 2
    |}

execute foo

console.log("=====================================\n")


// ============================
// function with anonymous type
// ============================

console.log("========= unitParameter test ============")
console.log("'export function unitParameter(options: { foo: string | number }): (v: number) => number':")

// wrong!
type Options () =
    member val foo : U2<string, float> = !^"asd" with get, set

// ok!
type OptionsR =
    {
        foo : U2<string, float>
    }

[<Import("unitParameter", from="./${outDir}/../../../NodejsConsoleApp/lib/test.js")>]
let unitParameter: options: OptionsR -> (float -> float) = jsNative

console.log("U2 with float: ", unitParameter {foo = !^13.} 12.2)


[<Import("unitParameter", from="./${outDir}/../../../NodejsConsoleApp/lib/test.js")>]
let unitParameter2: options: {| foo: U2<string, float> |} -> (float -> float) = jsNative

console.log("U2 with string: ", unitParameter2 {| foo = !^"asd" |} 12.2)

console.log("=====================================\n")



// ============================
// namespace
// ============================

console.log("========= namespace test ============")
[<Import("Func1", from="./${outDir}/../../../NodejsConsoleApp/lib/Helpers.js")>]
let func1: unit -> string = jsNative
console.log(func1 ())

[<Import("Func2", from="./${outDir}/../../../NodejsConsoleApp/lib/Helpers.js")>]
let func2: unit -> string = jsNative
console.log(func2 ())



open Elmish
open Elmish.React
open App
open Elmish.Navigation
open Elmish.UrlParser

Program.mkProgram init update (fun state dispatch -> app {| Model = state; Dispatch = dispatch |})
|> Program.toNavigable (parseHash Router.route) urlUpdate
|> Program.withReactBatched "root"
|> Program.withConsoleTrace
|> Program.run