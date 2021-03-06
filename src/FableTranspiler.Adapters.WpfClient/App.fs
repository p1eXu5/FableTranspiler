module FableTranspiler.Adapters.WpfClient.App

open System.IO
open Serilog
open Serilog.Extensions.Logging
open Elmish.WPF
open FableTranspiler.SimpleTypes
open FableTranspiler.Parsers.Types
open FableTranspiler.Ports.Persistence
open FableTranspiler.Adapters.Persistence
open MainModel

let main (window, settingsManager) =
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
        

    WpfProgram.mkProgram (fun () -> MainModel.init (StatementStore.create Statement.identifier) readFile settingsManager) update bindings
    |> WpfProgram.withLogger loggerFactory
    |> WpfProgram.startElmishLoop window