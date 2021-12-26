module App

open Elmish
open Feliz

type Model =
    {
        TestEntities: Components.TestEntity.TestEntityProps list
    }
    with
        static member Init() =
            {
                TestEntities = []
            }
            , Cmd.none

type Msg =
    | No


let update msg model =
    match msg with
    | _ -> model, Cmd.none


let app =
    React.functionComponent(
        "App",
        fun (props: {| Model: Model; Dispatch: Msg -> unit |}) ->
            Html.none
    )