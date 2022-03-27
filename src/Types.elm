module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict as Dict
import Element exposing (DeviceClass)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type alias Question =
    String


type alias Answer =
    ( String, Bool )


type alias Ratings =
    Dict.Dict Int Int


type alias Votes =
    List Int


type Role
    = Bigman
    | Normy


type alias UserName =
    Maybe String


type alias User =
    { name : UserName
    , sessionId : SessionId
    , ratings : Ratings
    , role : Role
    , jokes : List Joke
    }


type alias MetaUser =
    { user : User
    , votes : Votes
    }


type alias Joke =
    { id : Int
    , question : Question
    , answer : Answer
    , sessionId : SessionId
    }


type alias FrontendModel =
    { me : User
    , users : List User
    , ratedJokes : List ( Joke, Int )
    , deviceClass : DeviceClass
    , qInput : String
    , aInput : String
    , submitted : Bool
    }


type alias BackendModel =
    { users : List MetaUser
    , votesPerUser : Int
    , jokesPerDay : Int
    }


type FrontendMsg
    = ScreenSizeSet Int Int
    | QInputChanged String
    | AInputChanged String
    | DisabledSubmitPressed
    | SubmitPressed
    | NoOpFrontendMsg


type ToBackend
    = UpdateUserName UserName
    | UpdateUserRole Role
    | UpdateUserVotes Votes
    | CreateUserJoke Joke
    | UpdateUserRatings Ratings
    | NoOpToBackend


type BackendMsg
    = ClientConnected SessionId ClientId
    | NoOpBackendMsg


type ToFrontend
    = MeUpdated User
    | UsersUpdated (List User)
    | NoOpToFrontend
