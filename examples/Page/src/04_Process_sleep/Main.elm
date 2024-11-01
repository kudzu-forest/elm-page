module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Page
import Process
import Task


type CounterMsg
    = Increment
    | Decrement


initiateCounter : Page.Program ()
initiateCounter =
    let
        init =
            0

        update msg model =
            case msg of
                Increment ->
                    model + 1

                Decrement ->
                    model - 1

        view model =
            [ H.button [ HE.onClick Decrement ] [ H.text "-" ]
            , H.text <| String.fromInt model
            , H.button [ HE.onClick Increment ] [ H.text "+" ]
            ]
                |> List.intersperse (H.br [] [])
                |> H.div []
    in
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }


type SayHelloMsg
    = SayHelloButtonClicked
    | SayHello


initiateSayHello : Page.Program ()
initiateSayHello =
    let
        init () =
            ( False, Cmd.none )

        update msg model =
            case msg of
                SayHelloButtonClicked ->
                    ( model
                    , Process.sleep 10000
                        |> Task.map
                            (\_ -> SayHello)
                        |> Task.perform identity
                    )

                SayHello ->
                    ( True, Cmd.none )

        view model =
            H.div []
                [ H.button [ HE.onClick SayHelloButtonClicked ]
                    [ H.text "greet" ]
                , H.text
                    (if model then
                        "Hello!"

                     else
                        ""
                    )
                , H.br [] []
                , H.text "There will be some response in 10 seconds."
                ]
    in
    Page.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { currentPage : Page.Model
    , pageInitiators : ( Page.Program (), Page.Program () )
    }


type Msg
    = PageUpdated Page.Msg
    | PageInitiated ( Page.Model, Cmd Page.Msg )
    | ChangePage


main : Program () Model Msg
main =
    let
        init () =
            update
                (Page.mapInit PageInitiated initiateSayHello ())
                (Model Page.empty ( initiateCounter, initiateSayHello ))

        subscriptions model =
            Page.mapSubscriptions PageUpdated
                model.currentPage

        update msg model =
            case msg of
                PageUpdated pmsg ->
                    let
                        ( newPage, pageCmd ) =
                            Page.update pmsg model.currentPage
                    in
                    ( { model | currentPage = newPage }
                    , Cmd.map PageUpdated pageCmd
                    )

                PageInitiated ( newPage, pageCmd ) ->
                    ( { model | currentPage = newPage }
                    , Cmd.map PageUpdated pageCmd
                    )

                ChangePage ->
                    let
                        ( a, b ) =
                            model.pageInitiators
                    in
                    update
                        (Page.mapInit PageInitiated a ())
                        { model | pageInitiators = ( b, a ) }

        view model =
            H.div []
                [ Page.mapView PageUpdated model.currentPage
                , H.br [] []
                , H.button [ HE.onClick ChangePage ]
                    [ H.text "move to another page" ]
                , H.br [] []
                , H.text <|
                    "Try greeting and quickly moving to another page. The returned message for greeting will be ignored."
                ]
    in
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
