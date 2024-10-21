module Main exposing (..)

import Browser
import Emitter exposing (Emitter)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import UserForm exposing (User)


type alias Model =
    { userForm : Emitter Msg
    , user : Maybe User
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        emitterMsg =
            UserForm.initiate UserSubmitted ()

        ( emt, _, _ ) =
            Emitter.unwrapMsg emitterMsg
    in
    update (FormUpdated emitterMsg)
        { userForm = emt, user = Nothing }


type Msg
    = FormUpdated (Emitter.Msg Msg)
    | UserSubmitted User


subscriptions : Model -> Sub Msg
subscriptions model =
    Emitter.subscribe FormUpdated model.userForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormUpdated emitterMsg ->
            let
                ( emt, cmdInner, cmdMsg ) =
                    Emitter.unwrapMsg emitterMsg
            in
            ( { model | userForm = emt }
            , Cmd.batch
                [ Cmd.map FormUpdated cmdInner
                , cmdMsg
                ]
            )

        UserSubmitted u ->
            ( { model | user = Just u }, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.style "border" "ridge 3px pink" ]
            [ Emitter.mapView FormUpdated model.userForm ]
        , case model.user of
            Nothing ->
                H.text ""

            Just user ->
                H.text <|
                    "Hello, "
                        ++ user.name
                        ++ "!"
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
