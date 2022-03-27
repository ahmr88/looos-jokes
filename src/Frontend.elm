module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Events as BE
import Browser.Navigation as Nav
import Dict as Dict
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Lamdera exposing (sendToBackend)
import List as List
import String as Str
import Task
import Types exposing (..)
import Url
import Util exposing (..)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = \_ -> NoOpFrontendMsg
        , onUrlChange = \_ -> NoOpFrontendMsg
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Loos Jokes"
                , body = [ view model ]
                }
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { me = emptyNormy ""
      , users = []
      , ratedJokes = []
      , deviceClass = Phone
      , qInput = ""
      , aInput = ""
      , submitted = False
      }
    , Task.perform
        (\v -> ScreenSizeSet (round v.scene.width) (round v.scene.height))
        Dom.getViewport
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        ScreenSizeSet x y ->
            ( { model
                | deviceClass =
                    let
                        dev =
                            classifyDevice { width = x, height = y }
                    in
                    dev.class
              }
            , Cmd.none
            )

        QInputChanged v ->
            inputChanged (\m -> { m | qInput = v }) model

        AInputChanged v ->
            inputChanged (\m -> { m | aInput = v }) model

        SubmitPressed ->
            ( { model | qInput = "", aInput = "" }
            , sendToBackend <|
                CreateUserJoke
                    { id = 0
                    , question = model.qInput
                    , answer = ( model.aInput, False )
                    , sessionId = model.me.sessionId
                    }
            )

        DisabledSubmitPressed ->
            ( { model | submitted = True }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


inputChanged updater model =
    ( updater { model | submitted = False }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        MeUpdated me ->
            ( { model | me = me }, Cmd.none )

        UsersUpdated users ->
            ( { model | users = users, ratedJokes = sortRatedJokes users }, Cmd.none )

        NoOpToFrontend ->
            ( model, Cmd.none )


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Sub.batch [ BE.onResize ScreenSizeSet ]


view : Model -> Html FrontendMsg
view model =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Just (rgb255 61 64 81)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Font.family
            [ Font.sansSerif
            ]
        ]
    <|
        column
            [ Bg.color (rgb255 31 31 31)
            , Font.color (rgb255 163 177 138)
            , Font.semiBold
            , width fill
            , height fill
            , scrollbarY
            ]
            [ row [ width fill ] [ header ]
            , row [ width fill, height fill ] [ content model ]
            ]


header =
    row
        [ width fill
        , height fill
        , padding 15
        , Border.width 4
        , Font.color (rgb255 162 165 185)
        , Font.size 28
        ]
    <|
        [ el [ width fill ] <| text "Dad Joke of the Day"
        , el [] <| text "5"
        ]


content model =
    row
        [ width fill
        , height fill
        , spacing 7
        ]
    <|
        case model.deviceClass of
            Phone ->
                phoneContent model

            _ ->
                desktopContent model


phoneContent model =
    [ leftCol model ]


desktopContent model =
    let
        bot x =
            { bottom = x
            , top = 0
            , left = 0
            , right = 0
            }
    in
    [ leftCol model
    , rightCol
    ]


botBorder x =
    { bottom = x
    , top = 0
    , left = 0
    , right = 0
    }


colAttrs =
    [ height fill, width fill, padding 12, spacing 12 ]


leftCol model =
    column colAttrs
        [ row
            [ width fill
            , paddingEach (botBorder 10)
            , Border.widthEach
                (botBorder 2)
            , Border.dashed
            ]
            [ inputForm model.qInput model.aInput model.submitted ]
        , row [ width fill, height fill ]
            [ column
                [ height fill
                , width fill
                ]
                [ hiddenQs <| List.map (\( j, _ ) -> computeJokeInfo j model.me model.users) model.ratedJokes ]
            ]
        ]


rightCol =
    column colAttrs [ revealedQs ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (HE.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


inputForm qInput aInput submitted =
    let
        isDisabled =
            qInput == "" || aInput == ""

        submitEvent =
            if isDisabled then
                DisabledSubmitPressed

            else
                SubmitPressed

        errPlaceholder msg =
            Just <|
                Input.placeholder [ Font.color (rgba255 202 60 37 0.7) ] <|
                    text
                        (if isDisabled && submitted then
                            msg

                         else
                            ""
                        )

        inpAttr =
            [ padding 6
            , Bg.color (rgb255 61 61 61)
            , width (fillPortion 3)
            , focused <| [ Border.color (rgb255 163 177 138) ]
            , Border.width 1
            , Border.color (rgba 0 0 0 0)
            , htmlAttribute <| HA.attribute "dir" "auto" 
            ]
    in
    column
        [ width fill
        , spacing 7
        , Font.size 18
        , onEnter submitEvent
        ]
        [ row [ width fill ]
            [ Input.text inpAttr
                { label = Input.labelLeft [] <| text "Q:"
                , onChange = QInputChanged
                , placeholder = errPlaceholder "'Q' can't be empty"
                , text = qInput
                }
            ]
        , row [ width fill, spacing 10 ]
            [ Input.text inpAttr
                { label = Input.labelLeft [] <| text "A:"
                , onChange = AInputChanged
                , placeholder = errPlaceholder "'A' can't be empty"
                , text = aInput
                }
            , el [ width (fillPortion 1), height fill ] <|
                Input.button
                    [ Border.width 2
                    , Border.rounded 8
                    , width fill
                    , height fill
                    , centerY
                    , alpha
                        (if isDisabled then
                            0.5

                         else
                            1
                        )
                    ]
                    { onPress = Just submitEvent
                    , label = el [ centerX ] <| text "Submit"
                    }
            ]
        ]


revealedQs =
    text "revealed"


hiddenQs js =
    column [ width fill, height fill, spacing 7, clip, scrollbarY, padding 5] <|
        List.map qCard <|
            js


qCard ( j, uname, r ) =
    Input.button
        [ width fill
        , Bg.color (rgb255 25 25 25)
        , width fill
        , Border.width 0
        , Border.rounded 10
        , Font.size 16
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 1
            , blur = 4
            , color = rgb255 70 73 93
            }
        ]
        { onPress = Nothing
        , label =
            row
                [ width fill
                ]
                [ column
                    [ alignTop
                    , height fill
                    ]
                    [ row
                        [ Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 0
                            , top = 0
                            }
                        , Border.rounded 8
                        , Border.color (rgba255 163 177 138 0.5)
                        , alignTop
                        , padding 4
                        , Font.size 16
                        ]
                        [ text uname ]
                    , row [ height fill, paddingXY 5 0, Font.size 10 ] <|
                        List.map
                            (always <| text "✌")
                            (List.repeat 0 r)
                    ]
                , column [ width fill, paddingXY 10 20 ]
                    [ paragraph [width fill, htmlAttribute <| HA.attribute "dir" "auto"]
                        [ text j.question
                        ]
                    ]
                ]
        }
