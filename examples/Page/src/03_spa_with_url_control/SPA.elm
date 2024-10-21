module SPA exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode exposing (Value)
import Page exposing (Page)
import Pages.Book
import Pages.Buttons
import Pages.Forms
import Pages.Numbers
import Pages.Quotes
import Pages.TextFields
import Pages.Time
import Pages.WebSocket
import Url exposing (Url)
import Url.Parser exposing ((</>))



{-
   # What I did to create this SPA is :

   - gather .elm files from 'An Introduction to Elm'.(`Ctrl-a` and `Ctrl-v`)
   - put them in `./Pages` and rename the module.
   - substitute `Page` for `Browser`.
   - substitute `initiate` for `main`.
   - and wrote codes here!

-}


pages : List ( String, Page.Msg )
pages =
    [ ( "counter" ,  Pages.Buttons.initiate )
    , ( "reversing_text" , Pages.TextFields.initiate)
    , ( "form_control" , Pages.Forms.initiate)
    , ( "Book_through_Http" , Pages.Book.initiate ())
    , ( "Quotes_through_Json" , Pages.Quotes.initiate ())
    , ( "Random_dice" , Pages.Numbers.initiate ())
    , ( "What_time_is_it_now" , Pages.Time.initiate ())
    , ( "Web_socket" , Pages.WebSocket.initiate ())
    ]


pageDict : Dict String Page.Msg
pageDict =
    Dict.fromList pages


type alias Model =
    { page : Page
    , key : Nav.Key
    , title : String
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | PageUpdated Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key <|
                        Url.toString url
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                newPageMsg =
                    Url.Parser.parse urlParser url
                        |> Maybe.andThen
                            (\pageName -> Dict.get pageName pageDict)
                        |> Maybe.withDefault
                            Pages.Buttons.initiate
            in
            update (PageUpdated newPageMsg) model

        PageUpdated pmsg ->
            let
                ( p, pcmd ) =
                    Page.unwrapMsg pmsg
            in
            ( { model | page = p }
            , Cmd.map PageUpdated pcmd
            )


view : Model -> Browser.Document Msg
view model =
    { title = model.title
    , body =
        [ [ pages
                |> List.map toLink
                |> List.intersperse (H.br [] [])
                |> H.div
                    [ HA.style "border" "ridge 3px skyblue"
                    , HA.style "margin-right" "30px"
                    , HA.style "padding" "10px"
                    ]
          , Page.mapView PageUpdated model.page
          ]
            |> H.div
                [ HA.style "display" "grid"
                , HA.style "grid-template-columns" "1fr 4fr"
                , HA.style "width" "100%"
                ]
        ]
    }


toLink : ( String, pair ) -> Html Msg
toLink ( url, _ ) =
    H.a
        [ HA.href <| "/" ++ url ]
        [ H.text url ]


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        title =
            Url.Parser.parse urlParser url
                |> Maybe.withDefault "page not found"
    in
    update (PageUpdated Pages.Buttons.initiate)
    { page = Page.empty
    , key = key
    , title = title
    }
    


subscriptions : Model -> Sub Msg
subscriptions model =
    Page.subscribe PageUpdated model.page



-- Url --


urlParser : Url.Parser.Parser (String -> String) String
urlParser =
    Url.Parser.string


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
