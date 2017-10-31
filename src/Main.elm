module Main exposing (..)

import Html exposing (h1, p, text, div, node, input, tr,thead,tbody,td)
import Html.Attributes exposing (style)
import Plot exposing (..)
import Svg.Attributes as Attributes exposing (stroke, fill, class, r, x2, y2, style, strokeWidth, clipPath, transform, strokeDasharray)
import List.Extra exposing (..)
import Css exposing (width, maxWidth, minWidth, maxHeight,display, flex, px, pct)
import Html.Events exposing (onClick, onInput)
import Dict
import Round

styles =
    Css.asPairs >> Html.Attributes.style

-- MODEL


type alias Model =
    { hovering : Maybe Point
    , voteData : List (List PartyVote)
    , draftVoteData : List (List PartyVote)
    }

type alias PartyVote =
    (String, Int)

initialModel : Model
initialModel =
    let initVotes = 
          [ [  ("Party A", 15)
            ,  ("Party B", 85)
            ]
          , [  ("Party A", 53)
            ,  ("Party B", 47)
            ]
          , [  ("Party A", 53)
            ,  ("Party B", 47)
            ]
          , [  ("Party A", 53)
            ,  ("Party B", 47)
            ]
          , [  ("Party A", 53)
            ,  ("Party B", 47)
            ]
          ]
    in
        { hovering = Nothing
        , voteData = initVotes
        , draftVoteData = []
        }


-- UPDATE


type Msg
    = Hover (Maybe Point)
      | NewVotes Int (List PartyVote)
      | DraftVotes Int (String, Int)


update : Msg -> Model -> Model
update msg model =
    case msg of
      Hover point ->
        { model | hovering = point }
      NewVotes raceIndex votes ->
        { model | voteData = votesForRaceAtIndex raceIndex votes model.voteData}
      DraftVotes raceIndex partyVotes ->
        { model | draftVoteData = updatePartyInRace raceIndex partyVotes model.draftVoteData }

updatePartyInRace : Int -> PartyVote -> List (List PartyVote) -> List (List PartyVote)
updatePartyInRace raceIndex partyVote allRaces =
    List.indexedMap (\j partyVotes ->
                         if raceIndex == j then
                             partyVote :: ( List.filter (\(name,_) -> name /= fst partyVote) partyVotes )
                         else
                             partyVotes
                    )
        allRaces
        --this should be a map or array to make getting to a specific race easier


votesForRaceAtIndex : Int -> List PartyVote -> List (List PartyVote) -> List (List PartyVote)
votesForRaceAtIndex i newVotes model =
    List.indexedMap (\j raceVotes -> if i==j then newVotes else raceVotes) model

-- VIEW

fst (item,_) = item

viewRace : Int -> List PartyVote -> Html.Html Msg
viewRace raceIndex voteData =
    let
      wastedThreshold = List.map toFloat [wastedVoteThreshold voteData]

      vertAxis = customAxis <| \summary ->
        { position = closestToZero
        , axisLine = Just (simpleLine summary)
        , ticks = List.map simpleTick (wastedThreshold)
        , labels = List.map simpleLabel (wastedThreshold)
        , flipAnchor = False
        }

      settings =
        { defaultBarsPlotCustomizations
        | onHover = Just Hover
        , horizontalAxis = normalBarsAxis
        , grid = {horizontal=
                      customGrid <|
                      \summary ->
                          List.map (GridLineCustomizations [ stroke "#bbb" ]) <| wastedThreshold
                      , vertical= clearGrid}
        , margin = { top = 20, bottom = 30, left = 40, right = 40 }
        }

      unstackedGroup =
              stackedBars (List.map2 (hintGroup Nothing) (List.map fst (voteData))) --"Nothing" isn't useful here

    in
      div [ styles [  maxWidth (px 400), minWidth (px 200), flex Css.auto
                   ]]
        [ Plot.viewBarsCustom settings
              { unstackedGroup | areBarsStacked = True
              , axis = vertAxis
              }
              <| presentVotes voteData
        , voteControls raceIndex voteData
        ]

voteControls : Int -> List PartyVote -> Html.Html Msg
voteControls raceIndex votes =
    let
        tableRows = List.map (\{partyName, good, wasted} ->
                                         tr[]
                                          [ td[] [ text partyName ]
                                          , td[] [
                                                 input [
                                                        Html.Attributes.type_ "number"
                                                       , Html.Attributes.placeholder (toString <| (good + wasted))
                                                       , styles [width (pct 80)]
                                                       , onInput <| buildDraftVotes raceIndex partyName
                                                       -- , Html.Attributes.value model.currentSpelling
                                                       ][]]
                                          , td[] [ text << toString <| good]
                                          , td[] [ text << toString <| wasted]
                                          ]
                                    )
                         (calculateWastedVotes votes)
                             ++ [ tr[]
                                     [ td[][]
                                     , td[]
                                         [ input[Html.Attributes.type_ "submit"
                                                , onClick <| NewVotes raceIndex [("Partay",10),("Animal", 15)] ][text "update votes"]
                                         ]
                                     ]
                                ]

        buildDraftVotes raceIndex partyName newVoteString =
                case String.toInt newVoteString of
                            Err _ ->
                                DraftVotes raceIndex (partyName, 0)
                            Ok votes ->
                                DraftVotes raceIndex (partyName, votes)


        voteCounts =
            Html.table [][
                Html.thead []
                [ Html.tr []
                    [ Html.th[] [ text "Party Name"]
                    , Html.th[] [text "Total Votes"]
                    , Html.th[] [text "Good Votes"]
                    , Html.th[] [text "Wasted Votes"]
                    ]
                ]
                    , Html.tbody[]
                    tableRows
            ]
    in
        voteCounts


presentVotes : List PartyVote -> List (List Float)
presentVotes = calculateWastedVotes >> (List.map (\{partyName,good,wasted}-> [toFloat good, toFloat wasted]))

wastedVoteThreshold : List PartyVote -> Int
wastedVoteThreshold votes = ceiling ((toFloat <| List.sum <| List.map snd votes) / 2)

type alias PartyWastedVote =
    { partyName : String
    , good : Int
    , wasted : Int
    }

calculateWastedVotes : List PartyVote -> List PartyWastedVote
calculateWastedVotes votes =
    let loserVotes (name,votes) = {partyName=name,good=0,wasted=votes}

        winnerVotes (name, partyVotes) =
            if partyVotes > wastedVoteThreshold votes then
                 { partyName = name
                 , good = wastedVoteThreshold votes
                 , wasted = partyVotes-wastedVoteThreshold votes
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
        (\(name,vs)-> if name == fst (winner xs) then
                          winf (name,vs)
                      else
                          losef (name,vs)
        )
        xs

snd : (a,b) -> b
snd (_,item) = item

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }

view model =
    let summary =
            [ div[]
                  [ Html.h3 [styles [display Css.block]][text "Summary"]
                  , Html.table [styles [display Css.block]]
                      [ thead[]
                            [tr[]
                                 [ Html.th[][text "Parties"]
                                 , Html.th[][text "Efficiency Gaps"]
                                 ]
                            ]
                      , presentGaps model |> tbody []
                      ]
                  ]
             ]
    in
        (List.indexedMap viewRace model.voteData) ++
             summary
             |> Html.div [styles [Css.displayFlex, Css.flexWrap Css.wrap]]

presentGaps model =
       let presentRow total ((a,aData),(b,bData)) =
               if aData.wasted > bData.wasted then
                   presentRow total ((b,bData),(a,aData))
               else
                   tr[]
                       [ td[][text (a ++ " / " ++ b)]
                       , td[]
                           [ div[][text <| "(" ++ toString aData.wasted ++ "-" ++ toString bData.wasted ++ ")/" ++ toString total ++ " = "]
                           , div[][text << to2decimalPercent <| abs <| toFloat (aData.wasted - bData.wasted) / toFloat total]
                           ]
                       ]

           to2decimalPercent float =
               float * 100
                  |> Round.round 2
                  |> flip (++) "%"

           totalVoteMap =
               List.map calculateWastedVotes model.voteData
                   |> List.concat
                   |> List.foldl updateValue Dict.empty

           totalVotes =
               model.voteData
                   |> List.concat
                   |> List.map snd
                   |> List.sum


       in
                Dict.toList totalVoteMap |>
                allPairs |>
                List.map (presentRow totalVotes)

updateValue item dict =
               Dict.update item.partyName (\existing ->
       case existing of
           Just j ->
               Just { j | good=j.good + item.good, wasted=j.wasted + item.wasted }
           Nothing ->
               Just { good=item.good, wasted=item.wasted }
                                     ) dict

allPairs list =
    let allPairs_ parties acc =
        case parties of
            [] -> acc
            (x::xs) ->
                acc ++ List.map ( (,) x ) xs

    in
               List.foldl allPairs_ [] (List.Extra.tails list)
