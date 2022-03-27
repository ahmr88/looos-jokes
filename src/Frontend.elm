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
import Lamdera exposing (sendToBackend)
import List as List
import Task
import String as Str
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

        QInputChanged v -> ({model | qInput = v}, Cmd.none)
        AInputChanged v -> ({model | aInput = v}, Cmd.none)
        SubmitPressed -> (model, sendToBackend <| CreateUserJoke {id = 0, question = model.qInput, answer = (model.aInput, False), sessionId = model.me.sessionId})

        NoOpFrontendMsg ->
            ( model, Cmd.none )


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


phoneContent model = [leftCol model]


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
leftCol model = column colAttrs
        [ row
            [ width fill
            , paddingEach (botBorder 10)
            , Border.widthEach
                (botBorder 2)
            , Border.dashed
            ]
            [ inputForm ]
        , row [ width fill, height fill ] [ column [ height fill, width fill
        ] [ hiddenQs <| List.map (\(j,_) -> computeJokeInfo j model.me model.users) model.ratedJokes ] ]
        ]
rightCol = column colAttrs [ revealedQs ]


inputForm =
    column
        [ width fill
        , spacing 7
        , Font.size 18
        ]
        [ row [ width fill ]
            [ Input.text inpAttr
                { label = Input.labelAbove [] none
                , onChange = QInputChanged
                , placeholder = Nothing
                , text = ""
                }
            ]
        , row [ width fill ]
            [ Input.text inpAttr
                { label = Input.labelAbove [] none
                , onChange = AInputChanged
                , placeholder = Nothing
                , text = ""
                }
            , el [ width (fillPortion 1) ] <|
                Input.button
                    [ centerX
                    , padding 6
                    , Border.width 2
                    , Border.rounded 8
                    ]
                    { onPress =
                        Just SubmitPressed
                    , label = text "Submit"
                    }
            ]
        ]


inpAttr =
    [ padding 6, Bg.color (rgb255 61 61 61), width (fillPortion 3) ]


revealedQs =
    text "revealed"


hiddenQs js =
    column [ width fill, height fill, spacing 7, clip, scrollbarY ] <|
        List.map qCard <|
            js


qCard (j, uname, r) =
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
            , blur = 5
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
                    [ paragraph []
                        [ text j.question
                        ]
                    ]
                ]
        }



-- List.map (always <| row [] [text "ماهی"]) <|
-- List.range 1 30
