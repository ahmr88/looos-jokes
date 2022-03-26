module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict as Dict
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type alias Question =
    String


type alias Answer =
    ( String, Bool )

type alias Ratings =
  Dict.Dict Int Int

type Role = Bigman | Normy


type alias User =
    { name : Maybe String
    , sessionId : SessionId
    , ratings : Ratings
    , role : Role
    , jokes : List Joke
    }


type alias Joke =
    { id : Int
    , question : Question
    , answer : Answer
    , clientId : ClientId
    }


type alias FrontendModel =
    { me : User
    , users: List User
    }


type alias BackendModel =
    { users : List User
    }


type FrontendMsg
    = 
     NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = ClientConnected SessionId ClientId
    | NoOpBackendMsg

type ToFrontend
    = UpdateMe User
    | UsersUpdated (List User)
    | NoOpToFrontend
