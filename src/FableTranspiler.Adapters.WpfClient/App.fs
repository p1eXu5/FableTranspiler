module FableTranspiler.Adapters.WpfClient.App

open Elmish.WPF
open Serilog
open Serilog.Extensions.Logging
open MainModel
open FableTranspiler.Ports.Persistence
open FableTranspiler.Ports.PortsBuilder
open FableTranspiler.SimpleTypes
open System.IO

let main window =
    let logger =
        LoggerConfiguration()
          .MinimumLevel.Override("Elmish.WPF.Update", Events.LogEventLevel.Verbose)
          .MinimumLevel.Override("Elmish.WPF.Bindings", Events.LogEventLevel.Verbose)
          .MinimumLevel.Override("Elmish.WPF.Performance", Events.LogEventLevel.Verbose)
          .WriteTo.Console(outputTemplate="[{Timestamp:HH:mm:ss:fff} {Level:u3}] {Message:lj}{NewLine}{Exception}")
          .WriteTo.Seq("http://localhost:5341")
          .CreateLogger()

    let loggerFactory = new SerilogLoggerFactory(logger)
    //let store = Infrastruture.FsStatementInMemoryStore.store


    let readFile : ReadFileAsync =
        fun fullPath ->
            task {
                let fileName = fullPath |> FullPath.Value
                return File.OpenText(fileName)
            }
        

    let config =
        (
            FableTranspiler.Adapters.Persistence.StatementStore.create (),
            readFile
        )

    WpfProgram.mkProgram (MainModel.init ()) (fun msg m -> Ports.run config (update msg m)) bindings
    |> WpfProgram.withLogger loggerFactory
    |> WpfProgram.startElmishLoop window