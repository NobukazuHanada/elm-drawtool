module Diagrams exposing (
                          Diagrams,
                          Diagram,
                          Color,
                          KindOfShape,
                          KindOfShape(..),
                          scale,
                          translate,
                          toKindOfShape
                         )


type Diagrams = Diagrams (List Diagram)
type Diagram = Diagram {
        shape : Shape,
        position : Position,
        fill : Color,
        stroke : Color
    }
    | Group (List Diagram)

type Color = NoColor
           | RGB Int Int Int
           | RGBA Int Int Int Int


type Shape = Rect_ { width : Int, height : Int }
           | Circle_ { radius : Int }
           | Line_ { x : Int, y : Int }

type KindOfShape = Rect | Circle | Line

type alias Position = { x: Int, y: Int }
 
rect : Int -> Int -> Shape
rect width height =
    Rect_ { width = width, height = height }

circle : Int -> Shape
circle radius =
   Circle_ { radius = radius  }

line : Int -> Int -> Shape
line x y =
    Line_ {x = x, y = y}


translate : Int -> Int -> Diagram -> Diagram
translate x y diagram =
    case diagram of
        Diagram diagram ->
            Diagram { diagram |
                  position = { x = diagram.position.x + x,
                               y = diagram.position.y + y }
                    }
        Group group ->
                Group <| List.map (translate x y) group 

scale : Int -> Int -> Diagram -> Diagram
scale sx sy diagram =
    case diagram of
        Diagram diagram -> 
            case diagram.shape of
                Rect_ { width, height } ->
                    Diagram {
                        diagram |
                            shape = Rect_ { width = width + sx, height = height + sy }
                    }
                    
                Circle_ {radius} ->
                    Diagram {
                        diagram |
                            shape = Circle_ { radius = (radius + floor(sqrt(toFloat(sx * sx + sy * sy))))}
                                
                    }

                Line_ { x, y } ->
                    Diagram {
                        diagram |
                            shape = Line_ { x = x + sx, y = y + sy }
                    }

        Group group ->
            Group <| List.map (scale sx sy) group


toKindOfShape : Shape -> KindOfShape
toKindOfShape shape =
    case shape of
        Rect_ _ -> Rect
        Circle_ _ -> Circle
        Line_ _ -> Line
