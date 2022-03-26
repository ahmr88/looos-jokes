module Backend exposing (..)

import Dict as Dict
import Html
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import List as List
import Types exposing (..)
import Util exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { users = []
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sId cId ->
            let
                user =
                    List.head <| List.filter (\u -> u.sessionId == sId) model.users

                newUser =
                    emptyNormy sId

                newModel =
                    { model | users = newUser :: model.users }
            in
            case user of
                Just u ->
                    ( model, sendToFrontend cId <| UpdateMe u )

                Nothing ->
                    ( newModel
                    , Cmd.batch
                        [ sendToFrontend cId <| UpdateMe newUser
                        , broadcast <| UsersUpdated newModel.users
                        ]
                    )

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )


subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]
