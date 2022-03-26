module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict as Dict
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Lamdera
import List as List
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
        , subscriptions = \m -> Sub.none
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
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateMe me ->
          ({model | me = me}, Cmd.none)
        UsersUpdated users ->
          ({model | users = users},  Cmd.none)
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Html FrontendMsg
view model =
    layout
        [ Font.family
            [ Font.monospace
            ]
        ]
    <|
        column [] [ text "Hello, World" ]
