module Interactive exposing (..)

import Html exposing (h1, p, text, div, node, input, tr,thead,tbody,td)
import Html.Attributes exposing (style)
import Plot exposing (..)
import Svg.Attributes as Attributes exposing (stroke, fill, class, r, x2, y2, style, strokeWidth, clipPath, transform, strokeDasharray)
import List.Extra exposing (..)
import Css exposing (maxWidth, maxHeight,display, flex, px)

styles =
    Css.asPairs >> Html.Attributes.style

-- MODEL


type alias Model =
    { hovering : Maybe Point
    , voteData : List (List PartyVote)
    }

type alias PartyVote =
    (String, Int)

initialModel : Model
initialModel =
    { hovering = Nothing
    , voteData =
          [ [  ("Feelings Party", 100)
            ,  ("Nature Party", 50)
            ]
          , [  ("Feelings Party", 100)
            ,  ("Nature Party", 50)
            ]
          , [  ("Feelings Party", 100)
            ,  ("Nature Party", 500)
            ]
          ]
    }


-- UPDATE


type Msg
    = Hover (Maybe Point)
      | NewVotes (List (List PartyVote))


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

fst (item,_) = item

viewRace : List PartyVote -> Html.Html Msg
viewRace voteData =
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
      div [ styles [ maxWidth (px 400) , flex Css.auto]]
        [ Plot.viewBarsCustom settings
              { unstackedGroup | areBarsStacked = True
              , axis = vertAxis
              }
              <| presentVotes voteData
        , voteControls voteData
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

view model = Html.div [styles [Css.displayFlex]] <| List.map viewRace model.voteData
