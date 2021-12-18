module FableTranspiler.App

open Types

open Elmish.WPF
open Types
open Bindings
open Program
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

    WpfProgram.mkProgram Model.Init update bindings
    |> WpfProgram.withLogger (new SerilogLoggerFactory(logger))
    |> WpfProgram.startElmishLoop window