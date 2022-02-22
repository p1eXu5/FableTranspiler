module Router

type CodeStyle =
    | Fable
    | Feliz

type Page =
    | Home
    | ReactScroll of CodeStyle

open Elmish.UrlParser

let route : Parser<Page->Page,_> =
    oneOf [
        map Home (s "home")
        map (ReactScroll Fable) (s "react-scroll" </> s "fable")
        map (ReactScroll Feliz) (s "react-scroll" </> s "feliz")
    ]

let toHash = function
    | ReactScroll Fable -> "#react-scroll/fable"
    | ReactScroll Feliz -> "#react-scroll/feliz"
    | _ -> "#home"