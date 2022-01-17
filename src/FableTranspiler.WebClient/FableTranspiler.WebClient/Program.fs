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


// ==========
// interfaces
// ==========

type IFoo =
    abstract onDo : (unit -> unit) option
    abstract dutarion: U3<float, string, (float -> float)> option

// or [<Import("execute", from="./${outDir}/../../../NodejsConsoleApp/lib/test-interface.js")>]
[<Import("execute", from="./${outDir}/../../../NodejsConsoleApp/lib/test.js")>]
let execute(foo: IFoo) : unit = jsNative

let foo : IFoo = 
    !!{|
        onDo = (fun () -> console.log("onDo")) |> Some
        duration = fun n -> n * 2.f // 13, "13", fun n -> n * 2
    |}

execute foo



open Elmish
open Elmish.React
open App

Program.mkProgram Model.Init update (fun state dispatch -> app {| Model = state; Dispatch = dispatch |})
|> Program.withReactSynchronous "root"
|> Program.withConsoleTrace
|> Program.run