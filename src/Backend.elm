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
      , votesPerUser = 5
      , jokesPerDay = 4
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sId cId ->
            let
                user =
                    find (\u -> u.user.sessionId == sId) model.users

                newUser =
                    emptyMetaNormy sId

                newModel =
                    { model | users = newUser :: model.users }
            in
            case user of
                Just u ->
                    ( model
                    , Cmd.batch
                        [ sendToFrontend cId <| MeUpdated u.user
                        , sendToFrontend cId <| UsersUpdated <| List.map .user model.users
                        ]
                    )

                Nothing ->
                    ( newModel
                    , Cmd.batch
                        [ sendToFrontend cId <| MeUpdated newUser.user
                        , broadcast <| UsersUpdated <| List.map .user newModel.users
                        ]
                    )

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UpdateUserName n ->
            updateUser sessionId
                clientId
                (\u -> { u | user = (\uu -> { uu | name = n }) u.user })
                model

        UpdateUserRole r ->
            updateUser sessionId
                clientId
                (\u -> { u | user = (\uu -> { uu | role = r }) u.user })
                model

        UpdateUserRatings r ->
            updateUser sessionId
                clientId
                (\u -> { u | user = (\uu -> { uu | ratings = r }) u.user })
                model

        UpdateUserVotes votes ->
            let
                canVote u =
                    List.length votes < model.votesPerUser
            in
            updateUser sessionId
                clientId
                (\u ->
                    if canVote u then
                        { u | votes = votes }

                    else
                        u
                )
                model

        CreateUserJoke newJ ->
            let
                jokes =
                    List.map first <| sortRatedJokes <| List.map .user model.users

                nextId =
                    1
                        + (case List.maximum <| List.map (\j -> j.id) jokes of
                            Nothing ->
                                0

                            Just i ->
                                i
                          )
            in
            updateUser sessionId
                clientId
                (\uu ->
                    { uu
                        | user =
                            (\u -> { u | jokes = { newJ | id = nextId } :: u.jokes })
                                uu.user
                    }
                )
                model

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
                (\u ->
                    u.user.sessionId
                        == sid
                )
                updater
                model.users

        newUser =
            find (\u -> u.user.sessionId == sid) newModelUsers
    in
    ( { model | users = newModelUsers }
    , case newUser of
        Nothing ->
            Cmd.none

        Just u ->
            Cmd.batch
                [ sendToFrontend cid <| MeUpdated u.user
                , broadcast <| UsersUpdated <| List.map .user newModelUsers
                ]
    )
