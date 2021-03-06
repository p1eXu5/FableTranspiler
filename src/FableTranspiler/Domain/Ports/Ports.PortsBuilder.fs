namespace FableTranspiler.Ports

open System

module PortsBuilder =

    type Ports<'config, 'a> = Ports of action: ('config -> 'a)

    module Ports =
        /// Run a Interpreter with a given environment
        let run env (Ports action)  =
            action env  // simply call the inner function

        /// Create a Interpreter which returns the environment itself
        let ask = Ports id

        /// Map a function over a Reader
        let map f reader =
            Ports (fun env -> f (run env reader))

        /// flatMap a function over a Reader
        let bind f interpreter =
            let newAction env =
                let x = run env interpreter
                run env (f x)
            Ports newAction

        /// The sequential composition operator.
        /// This is boilerplate in terms of "result" and "bind".
        let combine expr1 expr2 =
            expr1 |> bind (fun () -> expr2)

        /// The delay operator.
        let delay<'config, 'a> (func: unit -> Ports<'config, 'a>) = func

        let retn v = (fun _ -> v) |> Ports

        let withEnv f interpreter =
            Ports (fun env -> run (f env) interpreter)

        let tryFinally compensation delayed =
            let action env =
                try
                    run env delayed
                finally
                    compensation ()
            Ports action

        let using (f: 'a -> Ports<_,_>) (v: #IDisposable) =
            tryFinally (fun () -> v.Dispose()) (f v)
    

    type PortsBuilder () =
        member _.Return(v) = Ports.retn v
        member _.ReturnFrom(expr) = expr
        member _.Bind(m: Ports<_,_>, f) = Ports.bind f m
        member _.Zero() = Ports (fun _ -> ())
        member _.Combine(expr1, expr2) = Ports.combine expr1 expr2
        member _.Delay(func) = Ports.delay func
        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body (), fun () ->
                this.While(guard, body))

        member _.TryFinally(body, compensation) = Ports.tryFinally compensation body
        member _.Using(v, f) = Ports.using f v
        member this.For(sequence: seq<_>, f) =
            this.Using(sequence.GetEnumerator(),fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> f enum.Current)))
        member _.Run(port) = port ()


    let ports = PortsBuilder()



module AsyncPortsBuilder =

    open System.Threading.Tasks
    open PortsBuilder


    type AsyncPorts<'config, 'a> = AsyncPorts of action: ('config -> Task<'a>)

    module AsyncPorts =
        /// Run a Interpreter with a given environment
        let run env (AsyncPorts action)  =
            action env  // simply call the inner function
            //|> Async.AwaitTask
            //|> Async.RunSynchronously

        /// Create a Interpreter which returns the environment itself
        let ask = Ports (fun env -> env) 

        /// Map a function over a Reader
        let map f reader =
            AsyncPorts (fun env -> f (run env reader))

        /// flatMap a function over a Reader
        let bind f interpreter =
            let newAction env =
                task {
                    let! x = run env interpreter
                    return! run env (f x)
                }
            AsyncPorts newAction

        /// The sequential composition operator.
        /// This is boilerplate in terms of "result" and "bind".
        let combine expr1 expr2 =
            expr1 |> bind (fun () -> expr2)

        /// The delay operator.
        let delay func = func()

        let retn v = (fun _ -> task { return v }) |> AsyncPorts

        let withEnv f interpreter =
            AsyncPorts (fun env -> run (f env) interpreter)

        let tryFinally compensation delayed =
            let action env =
                task {
                    try
                        return! run env delayed
                    finally
                        compensation ()
                }
            AsyncPorts action


        let using (f: 'a -> AsyncPorts<_,_>) (v: #IDisposable) =
            tryFinally (fun () -> v.Dispose()) (f v)


    type AsyncPortsBuilder () =
        member _.Return(v) = AsyncPorts.retn v
        member _.ReturnFrom(expr) = expr
        member _.Bind(m: AsyncPorts<_,_>, f) = AsyncPorts.bind f m
        member _.Bind(m: Task<_>, f: 'a -> AsyncPorts<_,_>) =
            AsyncPorts (fun env ->
                task {
                    let! x = m
                    return! AsyncPorts.run env (f x)
                }
            )
        member _.Bind(m: Ports<_,_>, f: 'a -> AsyncPorts<_,_>) =
            AsyncPorts (fun env ->
                task {
                    let x = Ports.run env m
                    return! AsyncPorts.run env (f x)
                }
            )

        member _.Zero() = AsyncPorts.retn ()
        member _.Combine(expr1, expr2) = AsyncPorts.combine expr1 expr2
        member _.Delay(func) = AsyncPorts.delay func
        member _.Using(v, f) = AsyncPorts.using f v


    let taskPorts = AsyncPortsBuilder()