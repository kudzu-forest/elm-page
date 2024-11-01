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
