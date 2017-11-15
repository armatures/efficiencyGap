module Race exposing
    ( Race
    , setPartyVotes
    , goodPartyVotes
    , wastedPartyVotes
    , totalVotes
    , wastedVoteThreshold
    , fromList
    , calculateWastedVotes
    , partyNames)

import Tuple

type Race = Race (List (String, Int))

fromList : (List (String, Int)) -> Race
fromList = Race

setPartyVotes : String -> Int -> Race -> Race
setPartyVotes partyName votes (Race race) =
    Race <| List.map (\(name, oldVotes) ->
                  if name == partyName then
                                       (name, votes)
                  else (name, oldVotes)
             ) race

goodPartyVotes : String -> Race -> Int
goodPartyVotes partyName race = case partyByName partyName race of
                                    Nothing -> 0
                                    Just party -> party.good

wastedPartyVotes : String -> Race -> Int
wastedPartyVotes partyName race = case partyByName partyName race of
                                    Nothing -> 0
                                    Just party -> party.wasted

partyByName : String -> Race -> Maybe PartyWastedVote
partyByName partyName race =
    calculateWastedVotes race
    |> List.filterMap (\party -> if party.partyName == partyName then
                                 Just party
                                 else Nothing)
    |> List.head


totalVotes : Race -> Int
totalVotes (Race race) = List.map Tuple.second race
                       |> List.sum

wastedVoteThreshold : Race -> Int
wastedVoteThreshold race =
    totalVotes race
        |> flip (//) 2
        |> (+) 1

partyNames : Race -> List String
partyNames (Race parties) = List.map Tuple.first parties

type alias PartyWastedVote =
    { partyName : String
    , good : Int
    , wasted : Int
    }

--need this for aggregating stats across all races...
calculateWastedVotes : Race -> List PartyWastedVote
calculateWastedVotes (Race votes) =
    let loserVotes (name,votes) = {partyName=name,good=0,wasted=votes}

        threshold = wastedVoteThreshold (Race votes)

        winnerVotes (name, partyVotes) =
            if partyVotes > threshold then
                 { partyName = name
                 , good = threshold
                 , wasted = partyVotes-threshold
                 }
            else
                 { partyName = name
                 , good = partyVotes
                 , wasted = 0
                 }
    in
         applyToWinner winnerVotes loserVotes votes

applyToWinner : ((String, Int) -> b) -> ((String, Int) -> b) -> List (String, Int) -> List b
applyToWinner winf losef xs =
    let
        winner xs =
            List.foldl (\(itemName, itemVal) (accName, accVal) ->
                            if itemVal > accVal then
                                (itemName, itemVal)
                            else
                                (accName, accVal)
                       )
                ("",0)
                    xs
    in
    List.map
        (\(name,vs)-> if name == Tuple.first (winner xs) then
                          winf (name,vs)
                      else
                          losef (name,vs)
        )
        xs
