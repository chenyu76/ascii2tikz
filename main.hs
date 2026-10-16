import Prelude

data Direction = Right | Up | Left | Down

data Position = Position
    { x :: Int
    , y :: Int
    }

data DiagramElement
    = Stroke
        { strokeStart :: Position
        , strokeEnd :: Position
        }
    | Arrow
        { arrowPosition :: Position
        , arrowDirection :: Direction
        }
    | Text
        { textPosition :: Position
        , textContent :: String
        }

type Diagram = [DiagramElement]

charToDiagram :: Position -> Char -> Diagram
charToDiagram p c = case c of
    _ -> []
