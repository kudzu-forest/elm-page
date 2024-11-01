module Page exposing
    ( Model, Msg, Program
    , empty, element, sandbox
    , mapInit, update, mapView, mapSubscriptions
    )

{-| This module allows you to combine multiple apps created with `Browser.element` or `Browser.sandbox` into a single application, with or without URL control. The key feature of the `Program` type is that, unlike other attempts to simplify SPA development, it does not use type variables for inner model and messages (it's `Program flag`, not `Program flag model msg`). This allows you to combine all the pages into a single `List`, `Dict`, `Random.Generator`, or any other data structure you like.

Users need to write a bit of boilerplate. For example, if you want to use the digital clock application (discussed in the official guide's subscription section), you'll need to set up a corresponding module like this. (This code is based on the one in the official guide, with first line added and `Page` substituted for `Browser` and `program` for `main`.)

    module DigitalClock exposing (program)

    -- Show the current time in your time zone.
    --
    -- Read how it works:
    --   https://guide.elm-lang.org/effects/time.html
    --
    -- For an analog clock, check out this SVG example:
    --   https://elm-lang.org/examples/clock
    --

    import Html exposing (..)
    import Page
    import Task
    import Time


    -- MAIN
    program =
        Page.element
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

    -- MODEL
    type alias Model =
        { zone : Time.Zone
        , time : Time.Posix
        }

    init : () -> ( Model, Cmd Msg )
    init _ =
        ( Model Time.utc (Time.millisToPosix 0)
        , Task.perform AdjustTimeZone Time.here
        )

    -- UPDATE
    type Msg
        = Tick Time.Posix
        | AdjustTimeZone Time.Zone

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Tick newTime ->
                ( { model | time = newTime }
                , Cmd.none
                )

            AdjustTimeZone newZone ->
                ( { model | zone = newZone }
                , Cmd.none
                )

    -- SUBSCRIPTIONS
    subscriptions : Model -> Sub Msg
    subscriptions model =
        Time.every 1000 Tick

    -- VIEW
    view : Model -> Html Msg
    view model =
        let
            hour =
                String.fromInt (Time.toHour model.zone model.time)

            minute =
                String.fromInt (Time.toMinute model.zone model.time)

            second =
                String.fromInt (Time.toSecond model.zone model.time)
        in
        h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]

You can use this page module as follows:

    module Main exposing (..)

    import Browser
    import DigitalClock
    import Html exposing (..)
    import Page

    main : Program () Model Msg
    main =
        Browser.element
            { init = init
            , subscriptions = subscriptions
            , update = update
            , view = view
            }

    type alias Model =
        { page : Page.Model
        }

    init : () -> ( Model, Cmd Msg )
    init _ =
        update
            (Page.mapInit PageInitialized DigitalClock.program ())
            { page = Page.empty }

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Page.mapSubscriptions GotPageMsg model.page

    type Msg
        = PageInitialized ( Page.Model, Cmd Page.Msg )
        | GotPageMsg Page.Msg

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            PageInitialized ( initialPage, initialCmd ) ->
                ( { model
                    | page = initialPage
                  }
                , Cmd.map GotPageMsg initialCmd
                )

            GotPageMsg pMsg ->
                let
                    ( newPage, newPageCmd ) =
                        Page.update pMsg model.page
                in
                ( { model
                    | page = newPage
                  }
                , Cmd.map GotPageMsg newPageCmd
                )

    view : Model -> Html Msg
    view model =
        Page.mapView GotPageMsg model.page

For usage with multiple pages, see [the GitHub repository](https://github.com/kudzu-forest/elm-page).


# CoreTypes

@docs Model, Msg, Program


# Creation

@docs empty, element, sandbox


# Usage

@docs mapInit, update, mapView, mapSubscriptions

-}

import Html exposing (Html)
import Unique exposing (Unique)


{-| A custom type representing internal states of your page.

  - The `Page.Model` is distinct from the `Model` defined in individual page modules.
      - `Page.Model` provides a level of abstraction that enables consistent handling of all your pages.
      - The values stored in your `Model` in each page module cannot be accessed from outside of that page.

-}
type Model
    = Model
        { key : Unique
        , html : Html Msg
        , sub : Sub Msg
        }


{-| A custom type representing `Msg`s in your page module. You need to use `Page.update` to access this value. See the example above.

  - The `Page.Msg` is distinct from the `Msg` defined in individual page modules.
      - `Page.Msg` provides a level of abstraction that enables consistent handling of all your pages.
      - Sending and receiving values across `Page`s are only possible with flags and ports. If your application needs more communication between pages, please take a look at `Emitter` module as well.
-}
type Msg
    = Updated (() -> ( Model, Cmd Msg ))


{-| A custom type representing the main program of a page. Values of this type can be passed to the `Page.mapInit` function with a flag.
-}
type Program flag
    = Program (flag -> ( Model, Cmd Msg ))


{-| Functions similarly to `Browser.element`. The return value is wrapped in `Page.Program flag` and can be passed to `Page.mapInit` along with a flag.
-}
element :
    { init : flag -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    }
    -> Program flag
element record =
    Program <|
        \flag ->
            let
                { init, subscriptions, view } =
                    record

                update_ =
                    record.update

                key =
                    Unique.unique ()

                ( initialModel, initialCmd ) =
                    init flag

                mapper : model -> msg -> Msg
                mapper model msg =
                    let
                        ( newModel, cmd ) =
                            update_ msg model
                    in
                    Updated
                        (\() ->
                            ( elementInner
                                { key = key
                                , model = newModel
                                , subscriptions = subscriptions
                                , view = view
                                , mapper = mapper
                                }
                            , Cmd.map (mapper newModel) cmd
                            )
                        )
            in
            ( elementInner
                { key = key
                , model = initialModel
                , subscriptions = subscriptions
                , view = view
                , mapper = mapper
                }
            , Cmd.map (mapper initialModel) initialCmd
            )


elementInner :
    { key : Unique
    , model : model
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , mapper : model -> msg -> Msg
    }
    -> Model
elementInner { key, model, subscriptions, view, mapper } =
    let
        specializedMapper =
            mapper model
    in
    Model
        { key = key
        , html = Html.map specializedMapper (view model)
        , sub = Sub.map specializedMapper (subscriptions model)
        }


{-| Functions similarly to `Browser.sandbox`. You can create a `Page.Program ()` easily by just using `Page.sandbox` instead of `Browser.sandbox`.
-}
sandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    }
    -> Program ()
sandbox record =
    element
        { init = \() -> ( record.init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = \msg model -> ( record.update msg model, Cmd.none )
        , view = record.view
        }


{-| Don't forget to use this in your `subscriptions` in the parent module!
-}
mapSubscriptions : (Msg -> appMsg) -> Model -> Sub appMsg
mapSubscriptions mapper (Model { sub }) =
    Sub.map mapper sub


{-| You can use your page with this function. See the example above.
-}
mapView :
    (Msg -> appMsg)
    -> Model
    -> Html appMsg
mapView mapper (Model { html }) =
    Html.map mapper html


{-| The only way to handle `Page.Msg` value from a child component.

- The message will be ignored and nothing happens if following condition is not satisfied.
    - the `Page.Msg` value as the first argument and the `Page.Model` value as the second argument belongs to __the same page__.
    - Here, __the same page__ means that they share the initialization `mapInit` function call. (check the `Process.sleep` example in the gitHub repository.)
    - In order to realize this feature, this module internally depends on [harrysarson/elm-hacky-unique](https://package.elm-lang.org/packages/harrysarson/elm-hacky-unique/1.0.0/).
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update (Updated lazyTriple) ((Model { key }) as oldPage) =
    let
        ( (Model rec) as newPage, pageCmd ) =
            lazyTriple ()

        msgKey =
            rec.key
    in
    if key == msgKey then
        ( newPage, pageCmd )

    else
        ( oldPage, Cmd.none )


{-| The only way you can start new page.
-}
mapInit : (( Model, Cmd Msg ) -> appMsg) -> Program flag -> flag -> appMsg
mapInit mapper (Program func) flag =
    mapper <| func flag


{-| An empty page. This can be used for initialization. See example above.
-}
empty : Model
empty =
    Model
        { key = Unique.unique ()
        , html = Html.text ""
        , sub = Sub.none
        }
