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

    let addTab interpreter =
        interpreter |> withEnv (fun (c, t) -> (c, t + 1))

    let apply mf m =
        let newAction env =
            let f = run env mf
            let x = run env m
            f x
        Interpreter newAction

    let traverse f xs =
        let (<*>) = apply
        let f1 a1 a2 = a1 :: a2
        
        List.foldBack (fun el state -> retn f1 <*> f el <*> state) xs (retn [])

    let sequence<'config, 'a> : Interpreter<'config, 'a> list -> Interpreter<'config, 'a list> = traverse id


module InterpreterBuilder =

    type InterpreterBuilder () =
        member _.Return(v) = Interpreter.retn v
        member _.ReturnFrom(interpreter) = interpreter
        member _.Bind(interpreter, f) = Interpreter.bind f interpreter
        member _.Zero() = Interpreter (fun _ -> ())
        member _.Combine(expr1, expr2) = Interpreter.combine expr1 expr2
        member _.Delay(func) = Interpreter.delay func
        
        //member this.TryFinaly(interpreter, compensation) =
        //    try this.ReturnFrom(interpreter)
        //    finally compensation ()

        //member this.Using(disposable: #System.IDisposable, expr: #System.IDisposable -> Interpreter<'config, 'a>) =
        //    let interpreter = expr disposable
        //    this.TryFinaly(interpreter, fun () ->
        //        match disposable with
        //        | null -> ()
        //        | disp -> disp.Dispose()
        //    )

        //member this.While(guard, interpreter) =
        //    if not (guard ()) then
        //        this.Zero()
        //    else 
        //        this.Bind(interpreter, this.ReturnFrom)


    let interpreter = InterpreterBuilder()