module Components.Fable.ReactScroll

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fable.ReactScroll
open Fable.ReactScroll.ScrollEvents
open Fable.ReactScroll.AnimateScroll
open Feliz.MaterialUI
open Fable.Core.JS
open Fable.ReactScroll.ScrollElement
open Fable.ReactScroll.ScrollLink
open Feliz


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




type Elem () =
    inherit Component<unit, unit>()
    override this.render() =
        Html.div [
            prop.ref (fun el -> this.props?parentBindings?domNode <- el)
            prop.children [
                Html.text "asd"
            ]
        ]
    
let customScrollElement = scrollElement ReactElementType.ofComponent<Elem,_,_>


let scrl =
    React.functionComponent (fun (props: {| children: seq<ReactElement> |}) ->
        Html.a [
            prop.children props.children
        ]
    )

type Scrl (props') =
    inherit Component<{| children: seq<ReactElement> |}, unit>(props')
    override this.render() =
        a [
            Fable.React.Props.OnClick this.props?onClick
            Fable.React.Props.ClassName this.props?className
        ] this.props?children


//let customScrollLink = scrollLink (ReactElementType.ofFunction scrl)
let customScrollLink2 = scrollLink ReactElementType.ofComponent<Scrl,_,_>

let foo = Browser.Dom.document.createElement("div")
console.log(foo)

let view =
    FunctionComponent.Of(
        (fun () ->
            let classes = useStyles ()

            React.useEffectOnce(fun () ->
                console.log ("========= scrollElement ================")
                //console.log (comp)
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

                                    customScrollLink2 [
                                        //ScrollLinkProps.Container foo;
                                        LinkProps.To "test4"
                                        LinkProps.Spy true
                                        LinkProps.Smooth !^true
                                    ] [Html.text "custom link"]
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

                    customScrollElement [ElementProps.Name "test4"] []

                    div [Style [Height "1000px"]] []
                ]
            ]
        ),
        "FableReactScroll"
    )