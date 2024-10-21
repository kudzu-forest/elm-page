module Minimal exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Events as HE
import Page exposing (Page)


type alias Model =
    { page : Page
    }


type Msg
    = PageUpdated Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageUpdated pmsg ->
            let
                ( newPage, pcmd ) =
                    Page.unwrapMsg pmsg
            in
            ( { page = newPage }
            , Cmd.map PageUpdated pcmd
            )


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ -> update (PageUpdated page) (Model Page.empty)
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


page : Page.Msg
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


view : Model -> Html Msg
view model =
    Page.mapView PageUpdated model.page
