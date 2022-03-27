module Backend exposing (..)

import Dict as Dict
import Html
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import List as List
import List.Extra exposing (find, updateIf)
import Tuple exposing (first)
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
                    find (\u -> u.sessionId == sId) model.users

                newUser =
                    emptyNormy sId

                newModel =
                    { model | users = newUser :: model.users }
            in
            case user of
                Just u ->
                    ( model
                    , Cmd.batch
                        [ sendToFrontend cId <| MeUpdated u
                        , sendToFrontend cId <| UsersUpdated model.users
                        ]
                    )

                Nothing ->
                    ( newModel
                    , Cmd.batch
                        [ sendToFrontend cId <| MeUpdated newUser
                        , broadcast <| UsersUpdated newModel.users
                        ]
                    )

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UpdateUserName n ->
            updateUser sessionId clientId (\u -> { u | name = n }) model

        UpdateUserRole r ->
            updateUser sessionId clientId (\u -> { u | role = r }) model

        UpdateUserRatings r ->
            updateUser sessionId clientId (\u -> { u | ratings = r }) model

        CreateUserJoke newJ ->
            let
                jokes =
                    List.map first <| sortRatedJokes model.users

                nextId =
                    1
                        + (case List.maximum <| List.map (\j -> j.id) jokes of
                            Nothing ->
                                0

                            Just i ->
                                i
                          )
            in
            updateUser sessionId clientId (\u -> { u | jokes = { newJ | id = nextId } :: u.jokes }) model

        NoOpToBackend ->
            ( model, Cmd.none )


subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]


updateUser sid cid updater model =
    let
        newModelUsers =
            updateIf
                (\uu ->
                    uu.sessionId
                        == sid
                )
                updater
                model.users

        newUser =
            find (\u -> u.sessionId == sid) newModelUsers
    in
    ( { model | users = newModelUsers }
    , case newUser of
        Nothing ->
            Cmd.none

        Just u ->
            Cmd.batch
                [ sendToFrontend cid <| MeUpdated u
                , broadcast <| UsersUpdated newModelUsers
                ]
    )
