import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Mouse

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias AppState =
    {
        diagrams : Diagrams,
        currentDrawTool : DrawTool
    }


type alias Diagrams = List Diagram
type alias Diagram = {
        shape : Shape,
        fill : Color,
        stroke : Color
    }

type Color = NoColor | Black 

type Shape = Rect Position Position | Circle Position Int | Line Position Position
type alias Position = { x : Int, y : Int }

type DrawTool = RectPen  Position Position | NoTool



type Event = 
    DrawStart Position | DrawMove Position | DrawEnd Position

init : (AppState, Cmd Event)
init = ({ diagrams = [], currentDrawTool = NoTool}, Cmd.none)

update : Event -> AppState -> (AppState, Cmd Event)
update event appState =
    (updateEvent event appState, Cmd.none)

updateEvent : Event -> AppState -> AppState
updateEvent event appState =
    case event of
        DrawStart pos ->
            {
                appState | currentDrawTool = RectPen pos pos
            }
        DrawMove pos ->
            {
                appState | currentDrawTool = case appState.currentDrawTool of
                                                 NoTool -> NoTool
                                                 RectPen sPos ePos -> RectPen sPos pos
            }
        DrawEnd pos ->

            {
                currentDrawTool = NoTool,
                diagrams =
                    case appState.currentDrawTool of
                        NoTool -> appState.diagrams
                        RectPen sPos ePos -> 
                            {
                                shape = Rect sPos ePos,
                                stroke = NoColor,
                                fill = NoColor
                            } :: appState.diagrams
            }


subscriptions appState =
    Sub.batch [
         Mouse.downs DrawStart,
         Mouse.moves DrawMove,
         Mouse.ups DrawEnd
        ]


view appState =
    currentDrawToolView appState.currentDrawTool ++ diagramsView appState.diagrams
        ++ [rect [x "0", y "0", width "1000", height "1000", fill "none", stroke "black"] []]
        |> svg
           [ width "1000", height "1000", viewBox "0 0 1000 1000" ]

        

diagramsView diagrams =
    diagrams
        |> List.map diagramView

diagramView diagram =
    let
        fillColor = diagram.fill
        strokeColor = diagram.stroke
        shape = diagram.shape
    in
        case shape of
            Rect startPos endPos ->
                rect [
                     x <| toString startPos.x,
                     y <| toString startPos.y,
                     width <| toString (endPos.x - startPos.x),
                     height <| toString (endPos.y - startPos.y),
                     fill "none",
                     stroke "black"
                    ] []
            Circle center radius ->
                circle [
                     x <| toString center.x,
                     y <| toString center.y,
                     r <| toString radius
                    ] []
            Line startPos endPos ->
                line [
                     x1 <| toString startPos.x,
                     y1 <| toString startPos.y,
                     x2 <| toString endPos.x,
                     y2 <| toString endPos.y
                    ] []

currentDrawToolView currentDrawTool =
    case currentDrawTool of
        NoTool -> []
        RectPen startPos endPos ->
            [
             rect [
                  x <| toString startPos.x,
                  y <| toString startPos.y,
                  width <| toString (endPos.x - startPos.x),
                  height <| toString (endPos.y - startPos.y),
                  fill "none",
                  stroke "black"
                 ] []
            ]
 
