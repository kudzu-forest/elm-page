module Main exposing (..)

import Browser
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



{-
   All I had to do to create this app is to:

   - gather .elm files from 'An Introduction to Elm' (`Ctrl-a` and `Ctrl-v`),
   - put them in `./Pages` and rename the module,
   - substitute `Page` for `Browser`,
   - substitute `page` for `main`,
   - and write codes here!

-}


pages : List ( String, ( Page, Cmd Page.Msg ) )
pages =
    [ ( "counter"
      , ( Pages.Buttons.page, Cmd.none )
      )
    , ( "reversing text"
      , ( Pages.TextFields.page, Cmd.none )
      )
    , ( "form control"
      , ( Pages.Forms.page, Cmd.none )
      )
    , ( "Book through Http"
      , Pages.Book.page ()
      )
    , ( "Quotes through Json"
      , Pages.Quotes.page ()
      )
    , ( "Random dice"
      , Pages.Numbers.page ()
      )
    , ( "What time is it now?"
      , Pages.Time.page ()
      )
    , ( "Web socket"
      , Pages.WebSocket.page ()
      )
    ]


type alias Model =
    { page : Page
    }


type Msg
    = PageInitialized ( Page, Cmd Page.Msg )
    | PageUpdated Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInitialized ( p, pcmd ) ->
            ( { model | page = p }
            , Cmd.map PageUpdated pcmd
            )

        PageUpdated pmsg ->
            let
                ( p, pcmd ) =
                    Page.unwrapMsg pmsg
            in
            ( { model | page = p }
            , Cmd.map PageUpdated pcmd
            )


view : Model -> Html Msg
view model =
    H.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "1fr 4fr"
        , HA.style "width" "100%"
        ]
        [ pages
            |> List.map toButton
            |> List.intersperse (H.br [] [])
            |> H.div
                [ HA.style "border" "ridge 3px skyblue"
                , HA.style "margin-right" "30px"
                , HA.style "padding" "10px"
                ]
        , Page.mapView PageUpdated model.page
        , H.text ""
        , H.br [] []
        , H.text ""
        , H.br [] []
        , H.text ""
        , H.br [] []
        , H.text ""
        , H.br [] []
        ]


toButton : ( String, ( Page, Cmd Page.Msg ) ) -> Html Msg
toButton ( name, pair ) =
    H.button
        [ HE.onClick (PageInitialized pair) ]
        [ H.text name ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Pages.Buttons.page }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Page.subscribe PageUpdated model.page


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
