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

displayName u = case u.name of
  Just n -> n
  Nothing -> roleToString u.role

computeJokeInfo j me us vs =
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
    { joke = j
    , author = case jOwner of
       Nothing -> j.sessionId
       Just {name, role} -> Maybe.withDefault (roleToString role) name
    , ratings = 0
    , voted = List.member j.id vs
    }

randomNames = ["Draconian Caper","Needy Ethiopian","Delirious Angolan","Interest Cambodian","Oafish Lollies","Attracted Italian","Somber Jamaican","Threatened Yemeni","Pathetic Australian","Obsessed Canadian","High Espresso","Idiotic Cookies","Aspiring Catnip","Bashful Gateau","Broad Dill","Sensual Israeli","Legitimate Relish","Lovesick Bulgarian","Idle Limes","Frazzled Bosnian","Uneasiness Armenian","Elliptical Bananas","Skillful Buttermilk","Phony Seaweed","Outraged Mexican","Abstracted Bolognase","Agressive Cypriot","Shabby Flatbread","Steel Pinto","Fulfilled Japanese","Ragefilled Nepalese","Loathsome Latvian","Ardent Tunisian","Confused Romanian","Merry Lebanese","Hideous Venison","Bewildered Egyptian","Ten Crumble","Tremendous Doughnut","Worried Syrian","Illiterate Oregano","Aromatic Pastry","Forgetful Pomegranates","Resentful Taiwanese","Amazed Brazilian","Forsaken Frybread","Perky Dressing","Ashamed Danish","Resigned Tanzanian","Quickest Vanilla","Useful Vinegar","Unfriendly Poached","Bouncy Cordial","Dopey Guyanan","Austere Vegetables","Wornout Indonesian","Digital Pasanda","Jealous Costarican","Morbid Albanian","Weary Guatemalan","Insane Slovakian","Luxuriant Sesame","Panicky Nigerian","Irritable Iraqi","Liberated Iranian","Illfated Vegan","Optimistic Kuwaiti","Insecure Slovak","Betrayed Sudanese","Disguised Shortbread","Pricey Canape","Fervent Pickles","Hateful Zambia","Shoddy Prawn","Fortunate Omelette","Submissive Indian","Mad Cuban","Exuberant Vietnamese","Aroused Jordanian","Courteous Smoothie","Observant Prunes","Annoying Pretzels","Delightful Kumquats","Sensitive Kenyan","Unbecoming Jellied","Astonished Venezuelan","Enigmatic Estonian","Serious Tapioca","Minor Muesli","Dreary Bolivian","Excluded Congolese","Dainty Sourdough","Dispirited Chilean","Dowdy Paprika","Selfish Korean","Hospitable Grapefruit","Ornate Crab","Artificial Mussels","Available Nutmeg","Illogical Scallops"]

