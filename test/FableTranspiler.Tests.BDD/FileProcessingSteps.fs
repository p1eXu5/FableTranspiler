module FileProcessingSteps

open NUnit.Framework
open TickSpec
open FableTranspiler.SimpleTypes



let [<Given>] ``a library located in "(.*)"`` (path: string) =
    LibLocation.Create path

let [<Given>] ``a "(.*)" file with content: (.*)`` (path: string) (content: string) =
    LibLocation.Create(path)