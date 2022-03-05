module Components.Fable.ReactScroll

open Fable.Core.JsInterop
open Fable.React
open Fable.ReactScroll
open Fable.ReactScroll.ScrollEvents
open Fable.ReactScroll.AnimateScroll
open Feliz
open Feliz.MaterialUI
open Fable.Core.JS


let useStyles : unit -> _ =
    Styles.makeStyles (fun styles _ ->
        {|
            element = styles.create [
                style.height (length.px 1000)
                style.backgroundColor "#ededed"
                style.fontSize (length.px 45)
                style.borderTop (length.px 1, borderStyle.solid, "000")
                style.paddingTop (length.px 55)
                style.paddingLeft (length.px 10)
            ]
        |}
    )

let view =
    FunctionComponent.Of(
        (fun () ->
            let classes = useStyles ()

            React.useEffectOnce(fun () ->
                console.log (events)
                events.scrollEvent.register "begin" (fun to' el ->
                    console.log ("begin", to')
                )
            )

            let scrollToBottom () =
                animateScroll.scrollToBottom None

            React.strictMode [
                fragment [] [
                    Mui.appBar [
                        appBar.position.static'
                        appBar.children [
                            Mui.toolbar [
                                toolbar.children [
                                    Link.link [
                                        LinkProps.To "test1"
                                        LinkProps.Spy true
                                        LinkProps.Smooth !^true
                                    ] [str "Test 1"]

                                    Link.link [
                                        LinkProps.To "test2"
                                        LinkProps.Spy true
                                        LinkProps.Smooth !^true
                                    ] [str "Test 2"]

                                    Link.link [
                                        LinkProps.To "test3"
                                        LinkProps.Spy true
                                        LinkProps.Smooth !^true
                                    ] [str "Test 3"]

                                    Html.a [
                                        prop.onClick (fun _ -> scrollToBottom ())
                                        prop.children [
                                            Html.text "Scroll To Bottom"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]

                    Element.element [
                        ElementProps.Name "test1"
                        Props.ClassName classes.element
                    ] [str "test 1"]

                    Element.element [
                        ElementProps.Name "test2"
                        Props.ClassName classes.element
                    ] [str "test 2"]

                    Element.element [
                        ElementProps.Name "test3"
                        Props.ClassName classes.element
                    ] [str "test 3"]
                ]
            ]
        ),
        "FableReactScroll"
    )