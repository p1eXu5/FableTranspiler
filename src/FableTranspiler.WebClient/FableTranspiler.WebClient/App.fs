module App

open Elmish
open Feliz
open Feliz.MaterialUI
open Fable.MaterialUI.Icons
open Router
open Fable.Core.JS
open Elmish.Navigation

type Model =
    {
        Page: Page
        TestEntities: Components.TestEntity.TestEntityProps list
    }

type Msg =
    | No




let urlUpdate (result: Page option) model =
    match result with
    | Some page ->
        { model with Page = page; }, Cmd.none

    | None ->
        console.error("Error parsing url")  
        ( model, Navigation.modifyUrl (toHash model.Page) )


let init result =
    urlUpdate result { Page = Home; TestEntities = [] }


let update msg model =
    match msg with
    | _ -> model, Cmd.none


let drawerWidth = 240


let useStyles : unit -> _ = Styles.makeStyles (fun styles theme ->
    {|
        root = styles.create [
            style.display.flex
        ]

        appBar = styles.create [
            style.custom ("width", sprintf "calc(100%% - %ipx)" drawerWidth)
            style.marginLeft (length.px drawerWidth)
        ]
        
        drawer = styles.create [
            style.width (length.px drawerWidth)
            style.flexShrink 0
        ]

        drawerPapaer = styles.create [
            style.width (length.px drawerWidth)
        ]

        toolbar = styles.create [
            yield! theme.mixins.toolbar // https://v4.mui.com/components/app-bar/#fixed-placement
        ]

        content = styles.create [
            style.flexGrow 1
            style.backgroundColor theme.palette.background.``default``
            style.padding (theme.spacing 3)
        ]
    |}
)


let viewPage model dispatch =
    match model.Page with
    | Home ->
        Mui.typography [
            typography.variant.h1
            typography.children "Fable Transpiler"
        ]
    
    | ReactScroll Fable ->
        Components.Fable.ReactScroll.view ()
    
    | ReactScroll Feliz ->
        Mui.typography [
            typography.variant.h1
            typography.children "NotImplemented"
        ]


let app =
    React.functionComponent(
        "App",
        fun (props: {| Model: Model; Dispatch: Msg -> unit |}) ->

            let classes = useStyles ()

            Html.div [
                prop.className classes.root
                prop.children [
                    Mui.cssBaseline []
                    
                    Mui.appBar [
                        appBar.position.fixed'
                        prop.className classes.appBar
                        appBar.children [
                            Mui.toolbar [
                                Mui.typography [
                                    typography.variant.h6
                                    typography.noWrap true
                                    typography.children "Permanent drawer"
                                ]
                            ]
                        ]
                    ]

                    Mui.drawer [
                        prop.className classes.drawer
                        drawer.variant.permanent
                        drawer.classes.paper classes.drawerPapaer
                        drawer.anchor.left
                        drawer.children [
                            Html.div [
                                prop.className classes.toolbar
                            ]

                            Mui.divider []

                            Mui.list [
                                list.children
                                    (
                                        [Page.Home; Page.ReactScroll Fable;]
                                        |> List.map (fun n ->
                                            let key = sprintf "%A" n
                                            Mui.listItem [
                                                listItem.button true
                                                listItem.component' "a"
                                                prop.href (Router.toHash n)
                                                prop.key key
                                                listItem.children [
                                                    Mui.listItemIcon [
                                                        mailIcon []
                                                    ]
                                                    Mui.listItemText [
                                                        listItemText.primary key
                                                    ]
                                                ]
                                            ]
                                        )
                                    )
                            ]
                        ]
                    ]

                    Html.main [
                        prop.className classes.content
                        prop.children [
                            Html.div [
                                prop.className classes.toolbar
                                prop.children [
                                ]
                            ]
                            viewPage props.Model props.Dispatch
                        ]
                    ]
                ]
            ]
    )