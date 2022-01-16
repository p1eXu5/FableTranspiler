module FableTranspiler.App

open Elmish.WPF
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

    let store = Infrastruture.FsStatementInMemoryStore.store

    WpfProgram.mkProgram Model.Init (update store) bindings
    |> WpfProgram.withLogger (new SerilogLoggerFactory(logger))
    |> WpfProgram.startElmishLoop window