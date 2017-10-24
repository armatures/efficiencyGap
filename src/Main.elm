module Interactive exposing (..)

import Html exposing (h1, p, text, div, node, input, tr,thead,tbody,td)
import Html.Attributes exposing (style)
import Plot exposing (..)
import Svg.Attributes as Attributes exposing (stroke, fill, class, r, x2, y2, style, strokeWidth, clipPath, transform, strokeDasharray)
import List.Extra exposing (..)


-- MODEL


type alias Model =
    { hovering : Maybe Point
    , voteData : List PartyVote
    }

type alias PartyVote =
    (String, Int)

initialModel : Model
initialModel =
    { hovering = Nothing
    , voteData =
          [  ("Winning Party", 100)
          ,  ("Losing Party", 50)
          ]
    }


-- UPDATE


type Msg
    = Hover (Maybe Point)
      | NewVotes (List PartyVote)


update : Msg -> Model -> Model
update msg model =
    case msg of
      Hover point ->
        { model | hovering = point }
      NewVotes votes ->
        { model | voteData = votes }


-- myDot : Maybe Point -> Point -> DataPoint msg
-- myDot hovering point =
--     hintDot (viewCircle 5 "#ff9edf") hovering point.x point.y


-- VIEW


view : Model -> Html.Html Msg
view model =
    let
      settings =
        { defaultBarsPlotCustomizations
        | onHover = Just Hover
        , horizontalAxis = normalBarsAxis
        , grid = {horizontal=
                      customGrid <|
                      \summary ->
                          List.map (GridLineCustomizations [ stroke "#bbb" ]) <| List.map toFloat [wastedVoteThreshold model.voteData]
                      , vertical= clearGrid}
        , margin = { top = 20, bottom = 30, left = 40, right = 40 }
        }

      unstackedGroup =
              stackedBars (List.map2 (hintGroup model.hovering) [ "g1", "g3", "g3", "g4", "g5" ])

    in
      div [ Html.Attributes.style [("max-height","1000px"), ("max-width","1000px")]]
        [ Plot.viewBarsCustom settings
              { unstackedGroup | areBarsStacked = True }
              <| presentVotes model.voteData
        , voteControls model.voteData
        ]

voteControls : List PartyVote -> Html.Html Msg
voteControls votes =
    let
        tableRows = List.map (\{partyName, good, wasted} ->
                                         tr[]
                                          [ td[] [ text partyName ]
                                          , td[] [ text << toString <| (good + wasted)]
                                          , td[] [ text << toString <| good]
                                          , td[] [ text << toString <| wasted]
                                          ]
                                    )
                         (calculateWastedVotes votes)
        voteCounts =
            Html.table []
                [ Html.th[] [ Html.td[][text "Party Name"]]
                , Html.th[] [Html.td[][text "Total Votes"]]
                , Html.th[] [Html.td[][text "Good Votes"]]
                , Html.th[] [Html.td[][text "Wasted Votes"]]
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
    let loserVotes =
            List.map (\(name,votes) -> {partyName=name,good=0,wasted=votes})

        winnerVotes (partyName, partyVotes) =
            if partyVotes > wastedVoteThreshold votes then
                 { partyName = partyName
                 , good = wastedVoteThreshold votes
                 , wasted = partyVotes-wastedVoteThreshold votes
                 }
            else
                 { partyName = partyName
                 , good = partyVotes
                 , wasted = 0
                 }
    in
        ((List.sortBy snd) >> List.reverse) votes
         |> (\votes_ -> case votes_ of
                     (v::vs) ->
                         (winnerVotes v :: loserVotes vs)
                     [] -> []
            )

snd : (a,b) -> b
snd (_,item) = item

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }
