module Interactive exposing (..)

import Html exposing (h1, p, text, div, node, input, tr,thead,tbody,td)
import Html.Attributes exposing (style)
import Plot exposing (..)
import Svg.Attributes as Attributes exposing (stroke, fill, class, r, x2, y2, style, strokeWidth, clipPath, transform, strokeDasharray)
import List.Extra exposing (..)


-- MODEL


type alias Model =
    { hovering : Maybe Point
    , voteData : List Int
    }


initialModel : Model
initialModel =
    { hovering = Nothing
    , voteData =
          [  100
          ,  50
          ]
    }


-- UPDATE


type Msg
    = Hover (Maybe Point)
      | NewVotes (List Int)


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

voteControls : List Int -> Html.Html Msg
voteControls votes =
    let
        tableRows = List.indexedMap (\partyNumber (goodVotes, wastedVotes) ->
                                         tr[]
                                          [ td[] [ text <| "Party "++ (toString partyNumber)]
                                          , td[] [ text << toString <| (goodVotes + wastedVotes)]
                                          , td[] [ text << toString <| goodVotes]
                                          , td[] [ text << toString <| wastedVotes]
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


presentVotes : List Int -> List (List Float)
presentVotes = calculateWastedVotes >> (List.map (\(a,b)-> [toFloat a, toFloat b]))

wastedVoteThreshold : List Int -> Int
wastedVoteThreshold votes = ceiling ((toFloat <| List.sum votes) / 2)

calculateWastedVotes : List Int -> List((Int, Int))
calculateWastedVotes votes =
    let loserVotes =
            List.map ((,) 0)
    in
        (List.sort >> List.reverse) votes
         |> (\votes_ ->
                 case votes_ of
                     (v::vs) ->
                         if v > wastedVoteThreshold votes then
                             (v-wastedVoteThreshold vs, wastedVoteThreshold vs)::loserVotes vs
                         else
                             (v,0)::loserVotes vs
                     [] -> []
            )
   -- List.foldl (\v -> ( if v>wastedVoteThreshold max 0 (v-wastedVoteThreshold votes), abs (wastedVoteThreshold) ))

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }
