import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Html.Events exposing (..)
import Mouse
import Json.Decode exposing (..)

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

type DrawTool = Drawing {
        start : Position,
        end : Position,
        shape : Shape
    } | Waiting Shape



type Event = 
    DrawStart Position | DrawMove Position | DrawEnd Position | ChangeShape Shape

-- shape utility ---

newRect = Rect { x = 0, y = 0} { x = 0, y = 0 }
newCircle = Circle { x = 0, y = 0} 0
newLine = Line { x = 0, y = 0} { x = 0, y = 0 }

updateShape start end shape =
    case shape of
        Rect _ _ -> Rect start end
        Circle _ _ -> Circle start (Basics.max
                                        (abs start.x - end.x)
                                        (abs start.y - end.y)
                                   )
        Line _ _ -> Line start end
---

newTool = Waiting newRect

init : (AppState, Cmd Event)
init = ({ diagrams = [], currentDrawTool = newTool}, Cmd.none)

update : Event -> AppState -> (AppState, Cmd Event)
update event appState =
    (updateEvent event appState, Cmd.none)

updateEvent : Event -> AppState -> AppState
updateEvent event appState =
    let
        drawTool = appState.currentDrawTool
    in
        case event of
            ChangeShape shape ->
                { appState | currentDrawTool = Waiting shape }
                    
            DrawStart pos ->
                case drawTool of
                    Waiting shape ->
                        {
                            appState | currentDrawTool =
                                Drawing {
                                    start = pos,
                                    end = pos,
                                    shape = updateShape pos pos shape
                                }
                        }
                    _ -> appState
            DrawMove pos ->
                case drawTool of
                    Drawing drawState -> 
                            {
                                appState |
                                    currentDrawTool =
                                    Drawing {
                                        start = drawState.start,
                                        end = pos,
                                        shape = updateShape drawState.start pos drawState.shape
                                    }
                            }
                    _ -> appState
            DrawEnd pos ->
                case drawTool of
                    Drawing drawState -> 
                        {
                            currentDrawTool = Waiting drawState.shape,
                                diagrams =  {
                                    shape = updateShape drawState.start pos drawState.shape,
                                        stroke = NoColor,
                                        fill = NoColor
                                } :: appState.diagrams
                        }
                    _ -> appState
                


subscriptions appState =
    Sub.none


view appState =
    div []
    [
     div [attribute "class" "tool area"] [toolArea appState],
     div [attribute "class" "draw-area"] [drawArea appState]
    ]

toolArea toolArea =
    div []
    [
     button [attribute "class" "tool-button",
             onClick <| ChangeShape newRect 
            ] [Html.text "rect"],
     button [attribute "class" "tool-button",
             onClick <| ChangeShape newCircle
            ] [Html.text "cirlce"],
     button [attribute "class" "tool-button",
             onClick <| ChangeShape newLine
            ] [Html.text "line"]
    ]


drawArea : AppState -> Html Event
drawArea appState =
    currentDrawToolView appState.currentDrawTool ++ diagramsView appState.diagrams
        ++ [rect [x "0", y "0", width "1000", height "1000", fill "none", stroke "black"] []]
        |>
          svg
          [ width "1000", height "1000", viewBox "0 0 1000 1000",
            on "mousedown" 
                (
                 Json.Decode.map DrawStart <|
                 Json.Decode.map2 Position 
                     (field "clientX" int)
                     (field "clientY" int)
                ),
            on "mousemove" 
                (
                 Json.Decode.map DrawMove <|
                 Json.Decode.map2 Position 
                     (field "clientX" int)
                     (field "clientY" int)
                ),
            on "mouseup" 
                (
                 Json.Decode.map DrawEnd <|
                 Json.Decode.map2 Position 
                     (field "clientX" int)
                     (field "clientY" int)
                )
          ]

diagramsView : Diagrams -> List (Svg Event) 
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
                     cx <| toString center.x,
                     cy <| toString center.y,
                     r <| toString radius,
                     fill "none",
                     stroke "black"
                    ] []
            Line startPos endPos ->
                line [
                     x1 <| toString startPos.x,
                     y1 <| toString startPos.y,
                     x2 <| toString endPos.x,
                     y2 <| toString endPos.y,
                     fill "none",
                     stroke "black"
                    ] []

currentDrawToolView : DrawTool -> List (Svg Event)
currentDrawToolView currentDrawTool =
    case currentDrawTool of
        Drawing drawState ->
            [case drawState.shape of 
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
                             cx <| toString center.x,
                             cy <| toString center.y,
                             r <| toString radius,
                             fill "none",
                             stroke "black"
                            ] []
                Line startPos endPos ->
                    line [
                         x1 <| toString startPos.x,
                             y1 <| toString startPos.y,
                             x2 <| toString endPos.x,
                             y2 <| toString endPos.y,
                             fill "none",
                             stroke "black"
                    ] []]
        _ -> []
 
