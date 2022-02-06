namespace FableTranspiler.VmAdapters.FsInterpreter

type Interpreter<'env, 'a> = Interpreter of action: ('env -> 'a)



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

    let retn v = (fun _ -> v) |> Interpreter

    let withEnv f interpreter =
        Interpreter (fun env -> run (f env) interpreter)


module InterpreterBuilder =

    type InterpreterBuilder () =
        member __.Return(v) = Interpreter.retn v
        member __.ReturnFrom(expr) = expr
        member __.Bind(x,f) = Interpreter.bind f x
        member __.Zero() = Interpreter (fun _ -> ())


    let interpreter = InterpreterBuilder()