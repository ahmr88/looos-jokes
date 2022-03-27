module Util exposing (..)

import Dict as Dict
import Lamdera exposing (ClientId, SessionId)
import List as List
import List.Extra as LE
import Maybe as Maybe
import Types exposing (..)


emptyNormy : SessionId -> User
emptyNormy sid =
    { name = Nothing
    , sessionId = sid
    , ratings = Dict.empty
    , jokes = []
    , role = Normy
    }

emptyMetaNormy : SessionId -> MetaUser
emptyMetaNormy sid = { user = emptyNormy sid, votes = []}


sortRatedJokes : List User -> List ( Joke, Int )
sortRatedJokes usrs =
    let
        cattedJokes =
            List.concatMap (\u -> u.jokes) usrs

        summedRating j =
            List.foldl (+) 0 <|
                List.concatMap
                    (\u ->
                        case Dict.get j.id u.ratings of
                            Nothing ->
                                []

                            Just i ->
                                [ i ]
                    )
                    usrs
    in
    List.map (\j -> ( j, summedRating j )) cattedJokes


roleToString r =
    case r of
        Normy ->
            "A Normy"

        Bigman ->
            "A Bigman"


computeJokeInfo : Joke -> User -> List User -> ( Joke, String, Int )
computeJokeInfo j me us =
    let
        jOwner =
            LE.find (\u -> u.sessionId == j.sessionId) us

        myVotes =
            case Dict.get j.id me.ratings of
                Nothing ->
                    0

                Just i ->
                    i
    in
    ( j
    , case jOwner of
       Nothing -> j.sessionId
       Just {name, role} -> Maybe.withDefault (roleToString role) name
    , myVotes
    )
