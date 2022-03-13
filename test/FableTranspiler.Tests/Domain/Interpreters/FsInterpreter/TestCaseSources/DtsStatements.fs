namespace FableTranspiler.Tests.Domain.Interpreters.FsInterpreter

[<AutoOpen>]
module private DtsStatements =
    [<Literal>]
    let private exportedInterfaceFormatted =
        """
            export interface FooProps {
                {field}
            }
        """

    let exportedInterfaceWith field =
        exportedInterfaceFormatted.Replace("{field}", field)