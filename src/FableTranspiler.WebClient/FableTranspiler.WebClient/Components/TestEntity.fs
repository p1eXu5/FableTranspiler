module Components.TestEntity

open Feliz


type TestEntityProps =
    {
        Name: string
        Run: unit -> string
    }


let testEntity =
    React.functionComponent(
        "TestEntity",
        fun (props: TestEntityProps) ->
            Html.none
        , 
        fun props -> props.Name
    )