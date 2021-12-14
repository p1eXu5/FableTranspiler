﻿module FableTranspiler.App

open Types

type Model = 
    {
        Actions: Command list
        Count: int
        StepSize: int 
    }

let init () = 
    { 
        Actions = [
            
        ]
        Count = 0
        StepSize = 1 
    }

type Msg =
    | Increment
    | Decrement
    | SetStepSize of int


let update msg m =
    match msg with
    | Increment -> { m with Count = m.Count + m.StepSize }
    | Decrement -> { m with Count = m.Count - m.StepSize }
    | SetStepSize x -> { m with StepSize = x }



open Elmish.WPF

let bindings () =
    [
        "CounterValue" |> Binding.oneWay (fun m -> m.Count)
        "Increment" |> Binding.cmd (fun m -> Increment)
        "Decrement" |> Binding.cmd (fun m -> Decrement)
        "StepSize" |> Binding.twoWay(
            (fun m -> float m.StepSize),
            (fun newVal m -> int newVal |> SetStepSize))
    ]




open Serilog
open Serilog.Extensions.Logging

let main window =
    let logger =
        LoggerConfiguration()
          .MinimumLevel.Override("Elmish.WPF.Update", Events.LogEventLevel.Verbose)
          .MinimumLevel.Override("Elmish.WPF.Bindings", Events.LogEventLevel.Verbose)
          .MinimumLevel.Override("Elmish.WPF.Performance", Events.LogEventLevel.Verbose)
          .WriteTo.Console()
          .CreateLogger()

    WpfProgram.mkSimple init update bindings
    |> WpfProgram.withLogger (new SerilogLoggerFactory(logger))
    |> WpfProgram.startElmishLoop window