namespace FableTranspiler.Interpreters.FsInterpreter

open FableTranspiler.Interpreters


type Interpreter<'config, 'a> = Interpreter of action: (('config * TabLevel) -> 'a)



module Interpreter =
    /// Run a Interpreter with a given environment
    let run env (Interpreter action)  =
        action env  // simply call the inner function

    /// Create a Interpreter which returns the environment itself
    let ask = Interpreter id

    /// Map a function over a Reader
    let map f reader =
        Interpreter (fun env -> f (run env reader))

    /// flatMap a function over a Reader
    let bind f interpreter =
        let newAction env =
            let x = run env interpreter
            run env (f x)
        Interpreter newAction


    /// The delay operator.
    let delay func = func()

    let retn v = (fun _ -> v) |> Interpreter

    let withEnv f interpreter =
        Interpreter (fun env -> run (f env) interpreter)

    /// The sequential composition operator.
    /// This is boilerplate in terms of "result" and "bind".
    let combine expr1 expr2 =
        expr1 |> bind (fun () -> expr2 )

module InterpreterBuilder =

    type InterpreterBuilder () =
        member __.Return(v) = Interpreter.retn v
        member __.ReturnFrom(expr) = expr
        member __.Bind(x,f) = Interpreter.bind f x
        member __.Zero() = Interpreter (fun _ -> ())
        member __.Combine(expr1, expr2) = Interpreter.combine expr1 expr2
        member __.Delay(func) = Interpreter.delay func


    let interpreter = InterpreterBuilder()