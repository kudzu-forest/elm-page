module Minimal exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Events as HE
import Page


type alias Model =
    { page : Page.Model
    }


type Msg
    = PageInitialized ( Page.Model, Cmd Page.Msg )
    | GotPageMsg Page.Msg


init : () -> ( Model, Cmd Msg )
init () =
    update
        (Page.mapInit PageInitialized page ())
        (Model Page.empty)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInitialized ( newPage, pageCmd ) ->
            ( { page = newPage }
            , Cmd.map GotPageMsg pageCmd
            )

        GotPageMsg pmsg ->
            let
                ( newPage, pageCmd ) =
                    Page.update pmsg model.page
            in
            ( { page = newPage }
            , Cmd.map GotPageMsg pageCmd
            )


view : Model -> Html Msg
view model =
    Page.mapView GotPageMsg model.page


subscriptions : Model -> Sub Msg
subscriptions model =
    Page.mapSubscriptions GotPageMsg model.page


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


page : Page.Program ()
page =
    Page.sandbox
        { init = 0
        , update =
            \_ model -> model + 1
        , view =
            \model ->
                H.div []
                    [ H.button [ HE.onClick () ] [ H.text "+" ]
                    , H.text <| String.fromInt model
                    ]
        }
