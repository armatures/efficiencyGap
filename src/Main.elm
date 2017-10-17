module Interactive exposing (..)

import Html exposing (h1, p, text, div, node)
import Html.Attributes exposing (style)
import Plot exposing (..)
import Svg.Attributes as Attributes exposing (stroke, fill, class, r, x2, y2, style, strokeWidth, clipPath, transform, strokeDasharray)
import List.Extra exposing (..)


-- MODEL


type alias Model =
    { hovering : Maybe Point }


initialModel : Model
initialModel =
    { hovering = Nothing }



-- UPDATE


type Msg
    = Hover (Maybe Point)


update : Msg -> Model -> Model
update msg model =
    case msg of
      Hover point ->
        { model | hovering = point }


-- myDot : Maybe Point -> Point -> DataPoint msg
-- myDot hovering point =
--     hintDot (viewCircle 5 "#ff9edf") hovering point.x point.y


-- VIEW

barData : List ( List Float )
barData =
  [ [ 1, 4 ]
  , [ 1, 5 ]
  , [ 2, 10 ]
  , [ 4, -2 ]
  , [ 5, 14 ]
  ]


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
                          List.map (GridLineCustomizations [ stroke "#bbb" ]) [8]
                      , vertical= clearGrid}
        , margin = { top = 20, bottom = 30, left = 40, right = 40 }
        }

      unstackedGroup =
              groups (List.map2 (hintGroup model.hovering) [ "g1", "g3", "g3", "g4", "g5" ])

    in
      div [ Html.Attributes.style [("max-height","1000px"), ("max-width","1000px")]]
        [ Plot.viewBarsCustom settings
              { unstackedGroup | areBarsStacked = True }
              barData
              , Plot.viewBarsCustom settings
              { unstackedGroup | areBarsStacked = False }
              barData
        ]

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }
