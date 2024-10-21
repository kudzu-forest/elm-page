module Emitter exposing
    ( Emitter, Msg
    , element
    , subscribe, mapView, unwrapMsg
    , empty
    )

{-| This module allows you to create a page module or UI component that can emit `Msg` to your app. If you haven't checked out the `Page` module in this package yet, it's recommended to start with `Page`, then move on to this module later. Some boilerplate code is required.


# Terminology

To simplify the explanations throughout this document, let's introduce some terminology:

  - `InnerMsg`
      - The message type you define for use within the `Emitter`.
  - `AppMsg`
      - The message type used by your application, which incorporates `Emitter AppMsg` as a component.
  - `EmittedValue`
      - A type that the component emits.
      - You will need to convert these values into `AppMsg` within the main app's code.


# Example Code

Here’s an example of how to define your user form component:

    module UserForm exposing (User, initiate)

    import Browser.Dom
    import Emitter exposing (Emitter)
    import Html as H exposing (Html)
    import Html.Attributes as HA
    import Html.Events as HE
    import Task

    type alias Model =
        { nameInput : String
        , ageInput : String
        , tried : Bool
        }

    init : () -> ( Model, Cmd Msg )
    init _ =
        ( { nameInput = ""
          , ageInput = ""
          , tried = False
          }
        , Cmd.none
        )

    type alias User =
        { name : String
        , age : Int
        }

    type Msg
        = NoOp
        | UserInputName String
        | UserInputAge String
        | SubmitButtonClicked
        | Emit User

    cmdEmission : User -> Cmd Msg
    cmdEmission user =
    -- This is the first piece of boilerplate code.
        Task.perform Emit (Task.succeed user)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            NoOp -> ( model, Cmd.none )

            UserInputName name ->
                ( { model | nameInput = name }, Cmd.none )

            UserInputAge age ->
                ( { model | ageInput = age }, Cmd.none )

            SubmitButtonClicked ->
                let
                    newModel = { model | tried = True }
                in
                if model.nameInput /= "" then
                    case String.toInt model.ageInput of
                        Just age ->
                            if 0 <= age && age <= 150 then
                                ( newModel, cmdEmission (User model.nameInput age) )
                            else
                                ( newModel, Browser.Dom.focus "ageInputField" |> Task.attempt (\_ -> NoOp) )

                        Nothing ->
                            ( newModel, Browser.Dom.focus "ageInputField" |> Task.attempt (\_ -> NoOp) )
                else
                    ( newModel, Browser.Dom.focus "nameInputField" |> Task.attempt (\_ -> NoOp) )

            Emit user ->
                -- This is the second piece of boilerplate code.
                -- You can copy and paste these lines!
                -- It may seem like it causes an infinite loop, but that's not the case because the `Cmd` is captured by the anonymous function passed below as the first argument of `Emitter.element`.
                ( model, cmdEmission user )

    view : Model -> Html Msg
    view model =
        H.div []
            [ if model.tried && model.nameInput == "" then
                H.span [ HA.style "color" "red" ] [ H.text "Your name must not be empty." ]
              else
                H.text ""
            , H.br [] []
            , H.input
                [ HA.id "nameInputField"
                , HA.type_ "text"
                , HA.placeholder "Input your name"
                , HA.value model.nameInput
                , HE.onInput UserInputName
                ]
                []
            , H.br [] []
            , H.span [ HA.style "color" "red" ]
                [ if not model.tried then
                    H.text ""
                  else if model.ageInput == "" then
                    H.text "Your age must not be empty."
                  else
                    case String.toInt model.ageInput of
                        Nothing -> H.text "Please input a valid integer."
                        Just age ->
                            if age < 0 then
                                H.text "Please come back after you are born."
                            else if age > 150 then
                                H.text "You should contact Guinness World Records about your longevity."
                            else
                                H.text ""
                ]
            , H.br [] []
            , H.input
                [ HA.id "ageInputField"
                , HA.type_ "text"
                , HA.placeholder "Input your age"
                , HA.value model.ageInput
                , HE.onInput UserInputAge
                ]
                []
            , H.br [] []
            , H.button [ HE.onClick SubmitButtonClicked ] [ H.text "Submit" ]
            ]

    initiate : (User -> appMsg) -> () -> Emitter.Msg appMsg
    initiate =
        Emitter.element
            (\msg ->
                case msg of
                    Emit user -> Just user

                    _ -> Nothing
            )
            { init = init
            , subscriptions = \_ -> Sub.none
            , update = update
            , view = view
            }

The module you defined can be used as follows:

    module Main exposing (..)

    import Browser
    import Emitter exposing (Emitter)
    import Html as H exposing (Html)
    import Html.Attributes as HA
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
        update (FormUpdated emitterMsg) { userForm = emt, user = Nothing }

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
                ( { model | userForm = emt }, Cmd.batch [ Cmd.map FormUpdated cmdInner, cmdMsg ] )

            UserSubmitted user ->
                ( { model | user = Just user }, Cmd.none )

    view : Model -> Html Msg
    view model =
        H.div []
            [ H.div [ HA.style "border" "ridge 3px pink" ] [ Emitter.mapView FormUpdated model.userForm ]
            , case model.user of
                Nothing ->
                    H.text ""

                Just user ->
                    H.text ("Hello, " ++ user.name ++ "!")
            ]

    main : Program () Model Msg
    main =
        Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


# Types

@docs Emitter, Msg


# Creation


@docs empty,element


# Usage

@docs subscribe, mapView, unwrapMsg



-}

import Html exposing (Html)
import Maybe.Extra
import Task


{-| A custom type that represents a UI module that can emit `AppMsg`. No type variables for its inner state or `InnerMsg` are needed.
-}
type Emitter appMsg
    = Emitter ( Html (Msg appMsg), Sub (Msg appMsg) )


{-| This `Emitter.Msg` value is used to pass through your `Emitter` and the entire application. See the example above for more details.
-}
type Msg appMsg
    = Updated (() -> ( Emitter appMsg, Cmd (Msg appMsg), Maybe appMsg ))


{-| Similar to `Page.element`, but with two additional arguments:

  - The first argument is a function that specifies how your component emits a value that can be captured outside the stream of `InnerMsg`.
      - This function monitors all `InnerMsg`s.
      - If the message is intended to emit a value to the main app, let it return `Just emittedValue`.
      - Otherwise, return `Nothing`.
  - The third argument is a converter from `EmittedValue` to `AppMsg`.
      - Typically, the first and second arguments are defined in the component module, while the third and fourth arguments are defined in the main app module.

Be sure to check the example code in the [GitHub repository](https://github.com/kudzu-forest/elm-page).
-}
element :
    (msg -> Maybe emitted)
    -> 
        { init : flag -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        }
    -> (emitted -> appMsg)
    -> flag
    -> Msg appMsg
element router_ { init, subscriptions, update, view } toAppMsg flag =
    let
        router : msg -> Maybe appMsg
        router =
            router_ >> Maybe.map toAppMsg

        mapper : model -> msg -> Msg appMsg
        mapper model innerMsg =
            let
                ( newModel, cmd ) = update innerMsg model
                mMsg = router innerMsg
            in
            Updated
                (\() ->
                    ( elementInner
                        { router = router
                        , mapper = mapper
                        , model = newModel
                        , subscriptions = subscriptions
                        , update = update
                        , view = view
                        }
                    , if Maybe.Extra.isJust mMsg then
                        Cmd.none
                        -- Preventing infinite loops.
                      else
                        Cmd.map (mapper newModel) cmd
                    , mMsg
                    )
                )

        ( initialModel, initialCmd ) = init flag
    in
    Updated <|
        \() ->
            ( elementInner
                { router = router
                , mapper = mapper
                , model = initialModel
                , subscriptions = subscriptions
                , update = update
                , view = view
                }
            , Cmd.map (mapper initialModel) initialCmd
            , Nothing
            )


elementInner :
    { router : msg -> Maybe appMsg
    , mapper : model -> msg -> Msg appMsg
    , model : model
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    }
    -> Emitter appMsg
elementInner { router, mapper, model, subscriptions, update, view } =
    let
        specializedMapper = mapper model
    in
    Emitter
        ( Html.map specializedMapper (view model)
        , Sub.map specializedMapper (subscriptions model)
        )


{-| Similar to `Page.subscribe`, this function maps over the subscriptions of the `Emitter`.
-}
subscribe : (Msg appMsg -> appMsg) -> Emitter appMsg -> Sub appMsg
subscribe mapper (Emitter ( _, sub )) =
    Sub.map mapper sub


{-| Similar to `Page.mapView`, this function maps over the view of the `Emitter`.
-}
mapView :
    (Msg appMsg -> appMsg)
    -> Emitter appMsg
    -> Html appMsg
mapView mapper (Emitter ( innerView, _ )) =
    Html.map mapper innerView


{-| Similar to `Page.unwrapMsg`, but the returned tuple includes, as its third component, a `Cmd msg` that inserts the emitted `AppMsg` into the main loop of the application.
-}
unwrapMsg : Msg appMsg -> ( Emitter appMsg, Cmd (Msg appMsg), Cmd appMsg )
unwrapMsg (Updated lazy) =
    let
        ( updated, innerCmd, maybeAppMsg ) = lazy ()

        emittedCmd =
            case maybeAppMsg of
                Nothing ->
                    Cmd.none

                Just msg ->
                    Task.perform identity (Task.succeed msg)
    in
    ( updated, innerCmd, emittedCmd )


{-| Similar to `Page.empty`, but requires two dummy values to pass type checking.
-}
empty : (a -> appMsg) -> b -> Emitter appMsg
empty toAppMsg flag =
    element
        (\_ -> Nothing)
        { init = \_ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = \_ _ -> ( (), Cmd.none )
        , view = \_ -> Html.text ""
        }
        toAppMsg
        flag
        |> unwrapMsg
        |> (\( e, _, _ ) -> e)
