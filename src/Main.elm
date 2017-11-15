module Main exposing (..)

import Html exposing (h1, p, text, div, node, input, tr,thead,tbody,td)
import Html.Attributes exposing (style)
import Plot exposing (..)
import Svg.Attributes as Attributes exposing (stroke, fill, class, r, x2, y2, style, strokeWidth, clipPath, transform, strokeDasharray)
import List.Extra exposing (..)
import Css exposing (width, maxWidth, minWidth, maxHeight,display, flex, em, px, pct)
import Html.Events exposing (onClick, onInput)
import Dict
import Round
import Array
import Race exposing (Race, fromList, setPartyVotes, goodPartyVotes, wastedPartyVotes, totalVotes, wastedVoteThreshold, partyNames)
import Tuple

styles =
    Css.asPairs >> Html.Attributes.style

-- MODEL


type alias Model =
    { hovering : Maybe Point
    , voteData : Array.Array Race
    , draftVoteData : Array.Array (List PartyVote)
    }

type alias PartyVote =
    (String, Int)

initialModel : Model
initialModel =
    let initVotes =
          Array.fromList
              [ Race.fromList [  ("Party A", 15) ,  ("Party B", 85),  ("Party C", 8)]
              , Race.fromList [  ("Party A", 53) ,  ("Party B", 47),  ("Party C", 4)]
              , Race.fromList [  ("Party A", 53) ,  ("Party B", 47),  ("Party C", 4)]
              , Race.fromList [  ("Party A", 53) ,  ("Party B", 47),  ("Party C", 4)]
              , Race.fromList [  ("Party A", 53) ,  ("Party B", 47),  ("Party C", 4)]
              ]
    in
        { hovering = Nothing
        , voteData = initVotes
        , draftVoteData =
            Array.fromList [ [  ("Party A", 15) ,  ("Party B", 85)]
                           , [  ("Party A", 53) ,  ("Party B", 47)]
                           , [  ("Party A", 53) ,  ("Party B", 47)]
                           , [  ("Party A", 53) ,  ("Party B", 47)]
                           , [  ("Party A", 53) ,  ("Party B", 47)]
                           ]
        }


-- UPDATE


type Msg
    = Hover (Maybe Point)
      | NewVotes Int
      | DraftVotes Int (String, Int)


update : Msg -> Model -> Model
update msg model =
    case msg of
      Hover point ->
        { model | hovering = point }
      NewVotes raceIndex ->
        { model | voteData = updateEditedParties raceIndex model}
      DraftVotes raceIndex partyVotes ->
        { model | draftVoteData = updatePartyInRace raceIndex partyVotes model.draftVoteData }

updatePartyInRace : Int -> PartyVote -> Array.Array (List PartyVote) -> Array.Array (List PartyVote)
updatePartyInRace raceIndex partyVote allRaces =
    case Array.get raceIndex allRaces of
        Nothing -> allRaces
        Just raceVotes -> Array.set raceIndex (partyVote :: ( List.filter (\(name,_) -> name /= Tuple.first partyVote) raceVotes )) allRaces


updateEditedParties : Int -> Model -> Array.Array Race
updateEditedParties i model =
    let setToDraftData race maybeDraftData =
            case maybeDraftData of
                Nothing -> race
                Just data -> fromList data
    in
        case Array.get i model.voteData of
                Nothing -> model.voteData
                Just race -> Array.set i (setToDraftData race (Array.get i model.draftVoteData)) model.voteData


-- VIEW

viewRace : (Int, Race) -> Html.Html Msg
viewRace (raceIndex, race) =
    let
      wastedThreshold = List.map toFloat [wastedVoteThreshold race]

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
              stackedBars (List.map2 (hintGroup Nothing) (partyNames (race))) --"Nothing" isn't useful here

    in
      div [ styles [  maxWidth (px 400), minWidth (px 200), flex Css.auto
                   ]]
        [ Plot.viewBarsCustom settings
              { unstackedGroup | areBarsStacked = True
              , axis = vertAxis
              }
              <| presentVotes race
        , voteControls raceIndex race
        ]

voteControls : Int -> Race -> Html.Html Msg
voteControls raceIndex race =
    let
        tableRows = List.map (\partyName ->
                                         tr[]
                                          [ td[] [ text partyName ]
                                          , td[] [
                                                 input [
                                                        Html.Attributes.type_ "number"
                                                       , Html.Attributes.placeholder (toString <| (goodPartyVotes partyName race + wastedPartyVotes partyName race))
                                                       , styles [width (pct 80)]
                                                       , onInput <| buildDraftVotes raceIndex partyName
                                                       ][]]
                                          , td[] [ text << toString <| goodPartyVotes partyName race]
                                          , td[] [ text << toString <| wastedPartyVotes partyName race]
                                          ]
                                    )
                         (partyNames race)
                             ++ [ tr[]
                                     [ td[][]
                                     , td[]
                                         [ input[Html.Attributes.type_ "submit"
                                                , Html.Attributes.value "update votes"
                                                ][]
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
        Html.form [ Html.Events.onSubmit <| NewVotes raceIndex ] [voteCounts]


presentVotes : Race -> List (List Float)
presentVotes race =
               partyNames race
                   |> List.map (\name->
                                     [toFloat <| goodPartyVotes name race
                                     , toFloat <| wastedPartyVotes name race
                                     ]
                                )

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }

view : Model -> Html.Html Msg
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
        (List.map viewRace <| Array.toIndexedList model.voteData) ++
             summary
             |> Html.div [styles [Css.displayFlex, Css.flexWrap Css.wrap]]

textCentered = Css.textAlign Css.center

showFraction numerator denominator = Html.span[styles[Css.padding (em 0.3)]]
               [ div[styles[textCentered, Css.borderBottom2 (px 2) Css.solid, Css.borderBottomColor (Css.rgb 0 0 0) ]
                    ][numerator]
               , div[styles[textCentered]][denominator ]
               ]

presentGaps : Model -> List (Html.Html Msg)
presentGaps model =
       let presentRow total ((a,aData),(b,bData)) =
               if aData.wasted > bData.wasted then
                   presentRow total ((b,bData),(a,aData))
               else
                   tr[]
                       [ td[][text (a ++ " / " ++ b)]
                       , td[styles [Css.displayFlex, Css.flexDirection Css.row, Css.alignItems Css.center]]
                           [ showFraction
                                 (text <| "(wasted " ++ a ++ ") - (wasted " ++ b ++ ")")
                                 (text <| "total")
                           , Html.span[][text " = "]
                           , showFraction
                                 (text <| "(" ++ toString bData.wasted ++ "-" ++ toString aData.wasted ++ ")")
                                 (text <| toString total)
                           , Html.span[][text " = "]
                           , Html.span[][text << to2decimalPercent <| toFloat (bData.wasted - aData.wasted) / toFloat total]
                           ]
                       ]

           to2decimalPercent float =
               float * 100
                  |> Round.round 2
                  |> flip (++) "%"

           allPartyNames =
               model.voteData
                   |> Array.toList
                   |> List.map partyNames
                   |> List.concat
                   |> List.Extra.unique

           partyTotalsAllRaces =
               model.voteData
                   |> Array.toList
                   |> List.foldl sumRaces Dict.empty
                   |> Dict.toList

           totalVotesAllRaces =
               model.voteData
                   |> Array.toList
                   |> List.map totalVotes
                   |> List.sum


       in
                partyTotalsAllRaces |>
                allPairs |>
                List.map (presentRow totalVotesAllRaces)

sumRaces : Race -> Dict.Dict String {good:Int,wasted:Int} -> Dict.Dict String {good:Int,wasted:Int}
sumRaces race dictionary =
    partyNames race
        |> List.foldl (updateParty race) dictionary

updateParty : Race ->  String -> Dict.Dict String {good:Int,wasted:Int} -> Dict.Dict String {good:Int,wasted:Int}
updateParty race name dict = Dict.update name (\existing ->
            case existing of
                Just j ->
            Just { j | good=j.good + goodPartyVotes name race, wasted=j.wasted + wastedPartyVotes name race }
                Nothing ->
            Just { good=goodPartyVotes name race, wasted=wastedPartyVotes name race }
                                           ) dict

allPairs : List a -> List (a,a)
allPairs list =
    let allPairs_ parties acc =
        case parties of
            [] -> acc
            (x::xs) ->
                acc ++ List.map ( (,) x ) xs

    in
               List.foldl allPairs_ [] (List.Extra.tails list)
