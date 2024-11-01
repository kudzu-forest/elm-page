module ManyPages exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode exposing (Value)
import Page
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
   - substitute `program` for `main`,
   - and write codes here!

-}


pages : List ( String, Msg )
pages =
    [ ( "counter", Pages.Buttons.program )
    , ( "reversing text", Pages.TextFields.program )
    , ( "form control", Pages.Forms.program )
    , ( "Book through Http", Pages.Book.program ) -- The () is a flag.
    , ( "Quotes through Json", Pages.Quotes.program )
    , ( "Random dice", Pages.Numbers.program )
    , ( "What time is it now?", Pages.Time.program )
    , ( "Web socket", Pages.WebSocket.program )
    ]
        |> List.map
            (Tuple.mapSecond <|
                \program ->
                    Page.mapInit PageInitialized program ()
            )


type alias Model =
    { page : Page.Model
    }


type Msg
    = PageUpdated Page.Msg
    | PageInitialized ( Page.Model, Cmd Page.Msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageUpdated pmsg ->
            let
                ( p, pcmd ) =
                    Page.update pmsg model.page
            in
            ( { model | page = p }
            , Cmd.map PageUpdated pcmd
            )

        PageInitialized ( p, pcmd ) ->
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


toButton : ( String, Msg ) -> Html Msg
toButton ( name, initializeMsg ) =
    H.button
        [ HE.onClick initializeMsg ]
        [ H.text name ]


init : () -> ( Model, Cmd Msg )
init _ =
    update (Page.mapInit PageInitialized Pages.Buttons.program ())
        { page = Page.empty }


subscriptions : Model -> Sub Msg
subscriptions model =
    Page.mapSubscriptions PageUpdated model.page


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
