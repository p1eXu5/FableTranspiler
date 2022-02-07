namespace FableTranspiler.Tests

open System
open Moq
open Microsoft.Extensions.Logging;


type MockLoggerFactory () =
    static member GetMockLogger<'T>(writeLine) =
        let (mockLogger: Mock< ILogger<'T>>) = new Mock< ILogger<'T> >();
        
        mockLogger
            .Setup( fun l -> 
                l.Log( It.IsAny<LogLevel>(),
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>(fun v t -> true), 
                    //It.IsAny<object>(),
                    It.IsAny<Exception>(), 
                    It.Is<Func<It.IsAnyType, Exception, string>>(fun v t -> true)
                    //It.IsAny<Func<object, Exception, string>>()
                ) 
            )
            .Callback<IInvocation>( fun invocation -> 
            
                let logLevel = invocation.Arguments[0] :?> LogLevel // The first two will always be whatever is specified in the setup above
                let _ = invocation.Arguments[1] :?> EventId  // so I'm not sure you would ever want to actually use them
                let state = invocation.Arguments[2]
                let exception' = invocation.Arguments[3] :?> Exception
                let formatter = invocation.Arguments[4]

                let message =
                    match formatter.GetType().GetMethod("Invoke") with
                    | null -> $"{logLevel.ToString().ToLowerInvariant()}: {typeof<'T>.FullName}"
                    | invokeMethod ->
                        $"{logLevel.ToString().ToLowerInvariant()}: {typeof<'T>.FullName}" + Environment.NewLine
                        + $"{invokeMethod.Invoke(formatter, [| state; exception' |]).ToString()}"

                writeLine message
            )
            |> ignore

        mockLogger

    static member GetMockLogger(context, writeLine) =
        let (mockLogger: Mock< ILogger>) = Mock< ILogger >();
        
        mockLogger
            .Setup( fun l -> 
                l.Log( It.IsAny<LogLevel>(),
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>(fun v t -> true), 
                    //It.IsAny<object>(),
                    It.IsAny<Exception>(), 
                    It.Is<Func<It.IsAnyType, Exception, string>>(fun v t -> true)
                    //It.IsAny<Func<object, Exception, string>>()
                ) 
            )
            .Callback<IInvocation>( fun invocation -> 
            
                let logLevel = invocation.Arguments[0] :?> LogLevel // The first two will always be whatever is specified in the setup above
                let _ = invocation.Arguments[1] :?> EventId  // so I'm not sure you would ever want to actually use them
                let state = invocation.Arguments[2]
                let exception' = invocation.Arguments[3] :?> Exception
                let formatter = invocation.Arguments[4]

                let message =
                    match formatter.GetType().GetMethod("Invoke") with
                    | null -> $"{logLevel.ToString().ToLowerInvariant()}"
                    | invokeMethod ->
                        $"{logLevel.ToString().ToLowerInvariant()}: %s{context}:" + Environment.NewLine
                        + $"\t%s{invokeMethod.Invoke(formatter, [| state; exception' |]).ToString()}"

                writeLine message
            )
            |> ignore

        mockLogger

    static member GetMockLoggerFactory(writeLine) =
        let mockFactory = Mock<ILoggerFactory>()

        mockFactory
            .Setup(fun f -> f.CreateLogger(It.IsAny<string>()))
            .Returns<string>(fun category -> MockLoggerFactory.GetMockLogger(category, writeLine).Object)
            |> ignore

        mockFactory