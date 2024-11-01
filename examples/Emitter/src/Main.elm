module Main exposing (..)

import Browser
import Emitter
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import UserForm exposing (User)


type alias Model =
    { userForm : Emitter.Model Msg
    , user : Maybe User
    }


init : () -> ( Model, Cmd Msg )
init _ =
    update
        ( Emitter.mapInit FormInitialized
            (UserForm.program UserSubmitted)
            ()
        )
        { userForm = Emitter.empty, user = Nothing }


type Msg
    = FormInitialized ( Emitter.Model Msg, Cmd (Emitter.Msg Msg), Cmd Msg )
    | GotFormMsg (Emitter.Msg Msg)
    | UserSubmitted User


subscriptions : Model -> Sub Msg
subscriptions model =
    Emitter.mapSubscriptions GotFormMsg model.userForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormInitialized ( emt, cmdInner, cmdMsg ) ->
            ( { model | userForm = emt }
            , Cmd.batch
                [ Cmd.map GotFormMsg cmdInner
                , cmdMsg
                ]
            )

        GotFormMsg m ->
            let( emt, cmdInner, cmdMsg ) =
                    Emitter.update m model.userForm
            in
            ( { model | userForm = emt }
            , Cmd.batch
                [ Cmd.map GotFormMsg cmdInner
                , cmdMsg
                ]
            )

        UserSubmitted u ->
            ( { model | user = Just u }, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.style "border" "ridge 3px pink" ]
            [ Emitter.mapView GotFormMsg model.userForm ]
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
