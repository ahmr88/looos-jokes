module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict as Dict
import Element exposing (DeviceClass)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)
import Set as Set


type alias Question =
    String


type alias Answer =
    ( String, Bool )




type alias Votes =
    Set.Set JokeId


type Role
    = Bigman
    | Normy


type alias UserName =
    Maybe String


type alias User =
    { name : UserName
    , sessionId : SessionId
    , likes : Set.Set JokeId
    , dislikes: Set.Set JokeId
    , role : Role
    , jokes : List Joke
    }


type alias MetaUser =
    { user : User
    , votes : Votes
    }

type alias JokeId = Int

type alias Joke =
    { id : JokeId
    , question : Question
    , answer : Answer
    , sessionId : SessionId
    }

type Tab = Editor | Viewer


type alias FrontendModel =
    { me : User
    , users : List User
    , ratedJokes : List ( Joke, Int )
    , deviceClass : DeviceClass
    , qInput : String
    , aInput : String
    , submitted : Bool
    , settings: Settings
    , votes: Votes
    , currentTab : Tab
    }


type alias BackendModel =
    { users : List MetaUser
    , votesPerUser : Int
    , jokesPerDay : Int
    , randomNames: List String
    }


type alias Settings =
    { maxVoteCount : Int
    }


type FrontendMsg
    = ScreenSizeSet Int Int
    | SwitchTab
    | QInputChanged String
    | AInputChanged String
    | DisabledSubmitPressed
    | SubmitPressed
    | CastVote Joke
    | NoOpFrontendMsg


type ToBackend
    = UpdateUserName UserName
    | UpdateUserRole Role
    | UpdateUserVotes Votes
    | CreateUserJoke Joke
    | NoOpToBackend


type BackendMsg
    = ClientConnected SessionId ClientId
    | NoOpBackendMsg


type ToFrontend
    = MeUpdated User Votes
    | UsersUpdated (List User)
    | SettingsChanged Settings
    | NoOpToFrontend
