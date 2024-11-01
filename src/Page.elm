module Page exposing
    ( Model, Msg, Program
    , empty, sandbox, element
    , mapInit, update, mapView, mapSubscriptions
    )

{-| This module allows you to combine multiple apps created with `Browser.element` or `Browser.sandbox` into a single application, with or without URL control. The key feature of the `Program` type is that, unlike other attempts to simplify SPA development, it does not use type variables for inner model and messages (it's `Program flag`, not `Program flag model msg`). This allows you to combine all the pages into a single `List`, `Dict`, `Random.Generator`, or any other data structure you like.

Users need to write a bit of boilerplate. For example, if you want to use the digital clock application (discussed in the official guide's subscription section), you'll need to set up a corresponding module like this. (This code is based on the one in the official guide, with first line added and `Page` substituted for `Browser` and `program` for `main`.)

For usage with multiple pages, see [the GitHub repository](https://github.com/kudzu-forest/elm-page).


# CoreTypes

@docs Model, Msg, Program


# Creation

@docs empty, sandbox, element


# Usage

@docs mapInit, update, mapView, mapSubscriptions

-}

import Html exposing (Html)
import Unique exposing (Unique)


{-| A custom type representing internal states of your page.
:::note info
The `Page.Model` is distinct from the `Model` defined in individual page modules. `Page.Model` provides a level of abstraction that enables consistent handling of all your pages.
:::
-}
type Model
    = Model
        { key : Unique
        , html : Html Msg
        , sub : Sub Msg
        }


{-| A custom type representing the main program of a page. Values of this type can be passed to the `Page.mapInit` function with a flag.
-}
type Program flag
    = Program (flag -> ( Model, Cmd Msg ))


{-| A custom type representing `Msg`s in your page module. You need to use `Page.update` to access this value. See the example above.
-}
type Msg
    = Updated (() -> ( Model, Cmd Msg ))


{-| Functions similarly to `Browser.element`. The return value is wrapped in `Page.Program` and can be passed to `Page.mapInit` along with a flag.
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
:::note warn
The `Page.Msg` value is ignored if the page which has emitted the `Page.Msg` value of first argument and `Model` value of second argument does not match.
For example, some delayed `Cmd Page.Msg` like `Process.sleep 10000 |> Task.perform (\_ -> SomeMsg)` is made then `Page.mapInit` is called before arrival of the returned `Page.Msg`.
:::
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
