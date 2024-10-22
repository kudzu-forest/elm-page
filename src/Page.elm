module Page exposing
    ( Page, Msg
    , unwrapMsg
    , empty, sandbox, element
    , mapView, subscribe
    )

{-| This module allows you to combine multiple apps created with `Browser.element` into a single application, with or without URL control. The key feature of the `Page` type is that, unlike other attempts to simplify SPA development, it does not use type variables (it's `Page`, not `Page model msg`). This allows you to combine all the pages into a single `List Page`, `Dict String Page`, `Random.Generator Page`, or any other data structure.

Users need to write a bit of boilerplate. For example, if you want to use the digital clock application (discussed in the official guide's subscription section), you'll need to set up a corresponding module like this. (This code is based on the one in the official guide, with `Page` substituted for `Browser` and `initiate` for `main`.)

    module Pages.Clock exposing (page)

    -- Show the current time in your time zone.
    --
    -- Read how it works:
    --   https://guide.elm-lang.org/effects/time.html
    --
    -- For an analog clock, check out this SVG example:
    --   https://elm-lang.org/examples/clock

    import Html exposing (..)
    import Page
    import Task
    import Time


    -- MAIN
    initiate =
        Page.element
            -- -main = Browser.element
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

And the main app looks like this:

    module Main exposing (..)

    import Browser
    import Html as H exposing (Html)
    import Html.Events exposing (onClick)
    import Pages.Clock

    type alias Model =
        { page : Page
        }

    type Msg
        = PageUpdated Page.Msg
        | PageReset

    init : () -> ( Model, Cmd Msg )
    init _ =
        let
            ( initialPage, initialCmd ) =
                Page.unwrapMsg <| Pages.Clock.initiate ()
        in
        ( Model initialPage
        , Cmd.map PageUpdated initialCmd
        )

    subscriptions : Model -> Sub Msg
    subscriptions model =
        -- Don't forget to add this line!
        Page.subscribe model.page

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            PageUpdated m ->
                -- These lines are boilerplate code.
                -- You can copy & paste them.
                let
                    ( newPage, newCmd ) =
                        Page.unwrapMsg m
                in
                ( { model
                    | page = newPage
                  }
                , Cmd.map PageUpdated newCmd
                )

            PageReset ->
                init ()

    view : Model -> Html Msg
    view model =
        H.div []
            [ Page.mapView PageUpdated model.page
            , H.br [] []
            , H.button [ onClick PageReset ] [ H.text "Reset" ]
            ]

    main : Program () Model Msg
    main =
        Browser.element
            { init = init
            , subscriptions = subscriptions
            , update = update
            , view = view
            }

For usage with multiple pages, see [the GitHub repository](https://github.com/kudzu-forest/elm-page).


# Types

@docs Page, Msg


# Handling Msg

@docs unwrapMsg


# Creation

@docs empty, sandbox, element


# Usage

@docs mapView, subscribe

-}

import Html exposing (Html)


{-| A custom type that can store the same information as `Browser.element`.
-}
type Page
    = Page ( Html Msg, Sub Msg )


{-| A `Msg` type emitted from the page module. You need to use `unwrapMsg` to access this value. See the example above.
-}
type Msg
    = Updated (() -> ( Page, Cmd Msg ))


{-| Similar to `Browser.element`. At initialization, you can pass a value of any type as flags. The resulting `(Page, Cmd Page.Msg)` will be returned wrapped in `Page.Msg`, so you need to `unwrap` the `Page.Msg` value like in the example above or call `update` during the initialization phase, as shown in examples in [the GitHub repository](https://github.com/kudzu-forest/elm-page). -}
element :
    { init : flag -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    }
    -> flag
    -> Msg
element { init, subscriptions, update, view } flag =
    let
        ( initialModel, initialCmd ) =
            init flag

        mapper model msg =
            let
                ( newModel, cmd ) =
                    update msg model
            in
            Updated
                (\() ->
                    ( elementInner
                        { model = newModel
                        , subscriptions = subscriptions
                        , update = update
                        , view = view
                        , mapper = mapper
                        }
                    , Cmd.map (mapper newModel) cmd
                    )
                )
    in
    Updated <|
        \() ->
            ( elementInner
                { model = initialModel
                , subscriptions = subscriptions
                , update = update
                , view = view
                , mapper = mapper
                }
            , Cmd.map (mapper initialModel) initialCmd
            )


elementInner :
    { model : model
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    , mapper : model -> msg -> Msg
    }
    -> Page
elementInner { model, subscriptions, update, view, mapper } =
    let
        specialMapper =
            mapper model
    in
    Page
        ( Html.map specialMapper (view model)
        , Sub.map specialMapper (subscriptions model)
        )


{-| Similar to `Browser.sandbox`. You can create a `Page` easily by just substituting `Page` for `Browser`.
-}
sandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    }
    -> Msg
sandbox { init, update, view } =
    element
        { init = \() -> ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
        ()


{-| Don't forget to use this in your `subscriptions` in the parent module!
-}
subscribe : (Msg -> appMsg) -> Page -> Sub appMsg
subscribe mapper (Page ( _, sub )) =
    Sub.map mapper sub


{-| You can use your page with this function. See the example above.
-}
mapView :
    (Msg -> appMsg)
    -> Page
    -> Html appMsg
mapView mapper (Page ( innerView, _ )) =
    Html.map mapper innerView


{-| The only way to handle messages from a child component.
-}
unwrapMsg : Msg -> ( Page, Cmd Msg )
unwrapMsg (Updated lazyPair) =
    lazyPair ()


{-| An empty page. useful for initialization.
-}
empty : Page
empty =
    sandbox
        { init = ()
        , update = \_ _ -> ()
        , view = \_ -> Html.text ""
        }
        |> unwrapMsg
        |> Tuple.first



