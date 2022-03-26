module Util exposing (..)

import Types exposing (..)
import Dict as Dict
import List as List
import Lamdera exposing (ClientId, SessionId)


emptyNormy : SessionId -> User
emptyNormy sid = { name = Nothing
             , sessionId = sid
             , ratings = Dict.empty
             , jokes = []
             , role = Normy
             }
