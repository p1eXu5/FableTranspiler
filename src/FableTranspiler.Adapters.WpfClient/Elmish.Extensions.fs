module Elmish.Extensions

/// <summary>
/// see <see href="https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state"/>
/// </summary>
type Operation<'T> =
    | Start
    | Finish of 'T

