module Emitter exposing
    ( Model, Msg, Program
    , empty, element
    , mapInit, mapSubscriptions, mapView, update
    )

{-| This module allows you to create a page module or UI component that can emit `Msg` to your app. If you haven't checked the `Page` module in this package yet, it's recommended to start with `Page`, then move on to this module later. Some boilerplate code is required.


# Terminology

To simplify the explanations throughout this document, let's introduce some terminology:

  - `InnerMsg`
      - The message type you define for use within the `Emitter.Program`.
  - `AppMsg`
      - The message type used by your application, which incorporates `Emitter.Program AppMsg` as a component.
  - `EmittedValue`
      - A type that the component emits.
      - You will need to convert these values into `AppMsg` within _the main app's code_.


# Example Code

Here’s an example of how to define your user form component:

    module UserForm exposing (User, program)

    import Browser.Dom
    import Emitter
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
            NoOp ->
                ( model, Cmd.none )

            UserInputName name ->
                ( { model | nameInput = name }, Cmd.none )

            UserInputAge age ->
                ( { model | ageInput = age }, Cmd.none )

            SubmitButtonClicked ->
                let
                    newModel =
                        { model | tried = True }
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
                        Nothing ->
                            H.text "Please input a valid integer."

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

    program : (User -> appMsg) -> Emitter.Program () appMsg
    program =
        Emitter.element
            (\msg ->
                case msg of
                    Emit user ->
                        Just user

                    _ ->
                        Nothing
            )
            { init = init
            , subscriptions = \_ -> Sub.none
            , update = update
            , view = view
            }

The module you defined can be used as follows:

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
            (Emitter.mapInit FormInitialized
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
                let
                    ( emt, cmdInner, cmdMsg ) =
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

If you want to try moving this code, `git clone` the corresponding repo.


# Types

@docs Model, Msg, Program


# Creation

@docs empty, element


# Usage

@docs mapInit, mapSubscriptions, mapView, update

-}

import Html exposing (Html)
import Maybe.Extra
import Task
import Unique exposing (Unique)


{-| A custom type that represents a UI module that can emit `AppMsg`. No type variables for its inner state or `InnerMsg` are needed.
-}
type Model appMsg
    = Model
        { key : Unique
        , html : Html (Msg appMsg)
        , sub : Sub (Msg appMsg)
        }


{-| This `Emitter.Msg` value is used both to update the emitter state and to emit some value for the entire application. See the example above for more details.
-}
type Msg appMsg
    = Updated (() -> ( Model appMsg, Cmd (Msg appMsg), Maybe appMsg ))


{-| A custom type representing the main program of your component module. This type of values can be passed to `Emitter.mapInit` with flag.
-}
type Program flag appMsg
    = Program (flag -> ( Model appMsg, Cmd (Msg appMsg), Cmd appMsg ))


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
    -> Program flag appMsg
element router_ rec toAppMsg =
    let
        { init, subscriptions, view } =
            rec

        update_ =
            rec.update
    in
    Program <|
        \flag ->
            let
                key : Unique
                key =
                    Unique.unique ()

                router : msg -> Maybe appMsg
                router =
                    router_ >> Maybe.map toAppMsg

                mapper : model -> msg -> Msg appMsg
                mapper model innerMsg =
                    let
                        ( newModel, cmd ) =
                            update_ innerMsg model

                        mMsg =
                            router innerMsg
                    in
                    Updated
                        (\() ->
                            ( elementInner
                                { key = key
                                , mapper = mapper
                                , model = newModel
                                , subscriptions = subscriptions
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

                ( initialModel, initialCmd ) =
                    init flag

                model_ =
                    elementInner
                        { key = key
                        , mapper = mapper
                        , model = initialModel
                        , subscriptions = subscriptions
                        , view = view
                        }
            in
            update
                (Updated <|
                    \() ->
                        ( model_
                        , Cmd.map (mapper initialModel) initialCmd
                        , Nothing
                        )
                )
                model_


elementInner :
    { key : Unique
    , mapper : model -> msg -> Msg appMsg
    , model : model
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> Model appMsg
elementInner { key, mapper, model, subscriptions, view } =
    let
        specializedMapper =
            mapper model
    in
    Model
        { key = key
        , html = Html.map specializedMapper (view model)
        , sub = Sub.map specializedMapper (subscriptions model)
        }


{-| Similar to `Page.mapInit`, but the returned tuple is a triple.
-}
mapInit : (( Model appMsg, Cmd (Msg appMsg), Cmd appMsg ) -> appMsg) -> Program flag appMsg -> flag -> appMsg
mapInit emitterInitialized (Program func) flag =
    emitterInitialized <| func flag


{-| Similar to `Page.mapSubscriptions`, this function maps over the subscriptions of the `Emitter.Program`.
-}
mapSubscriptions : (Msg appMsg -> appMsg) -> Model appMsg -> Sub appMsg
mapSubscriptions mapper (Model { sub }) =
    Sub.map mapper sub


{-| Similar to `Page.mapView`, this function maps over the view of the `Emitter.Program`.
-}
mapView :
    (Msg appMsg -> appMsg)
    -> Model appMsg
    -> Html appMsg
mapView mapper (Model { html }) =
    Html.map mapper html


{-| Similar to `Page.updated`, but the returned tuple includes, as its third component, a `Cmd msg` that inserts the emitted `AppMsg` into the main loop of the application.
-}
update : Msg appMsg -> Model appMsg -> ( Model appMsg, Cmd (Msg appMsg), Cmd appMsg )
update (Updated lazy) ((Model { key }) as oldModel) =
    let
        ( (Model rec) as updated, innerCmd, maybeAppMsg ) =
            lazy ()

        newKey =
            rec.key

        emittedCmd =
            case maybeAppMsg of
                Nothing ->
                    Cmd.none

                Just msg ->
                    Task.perform identity (Task.succeed msg)
    in
    if newKey == key then
        ( updated, innerCmd, emittedCmd )

    else
        ( oldModel, Cmd.none, emittedCmd )


{-| Similar to `Page.empty`.
-}
empty : Model appMsg
empty =
    Model
        { key = Unique.unique ()
        , html = Html.text ""
        , sub = Sub.none
        }
