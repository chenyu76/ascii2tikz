{-# LANGUAGE LambdaCase #-}

import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Prelude hiding (Left, Right)

data Direction = Right | Up | Left | Down
    deriving (Eq, Show)

directionToNegPosition :: Direction -> Position
directionToNegPosition = \case
    Up -> Position 0 (-1)
    Down -> Position 0 1
    Left -> Position (-1) 0
    Right -> Position 1 0

oppositeDirection :: Direction -> Direction
oppositeDirection = \case
    Up -> Down
    Down -> Up
    Left -> Right
    Right -> Left

directionToPosition :: Direction -> Position
directionToPosition = directionToNegPosition . oppositeDirection

data Position = Position
    { x :: Int
    , y :: Int
    }
    deriving (Eq, Show)

instance Num Position where
    Position x1 y1 + Position x2 y2 = Position (x1 + x2) (y1 + y2)
    Position x1 y1 - Position x2 y2 = Position (x1 - x2) (y1 - y2)
    Position x1 y1 * Position x2 y2 = Position (x1 * x2) (y1 * y2)
    negate (Position px py) = Position (negate px) (negate py)
    abs (Position px py) = Position (abs px) (abs py)
    signum (Position px py) = Position (signum px) (signum py)
    fromInteger n = Position (fromInteger n) (fromInteger n)

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
    deriving (Eq, Show)

type Diagram = [DiagramElement]

lineChars :: [[Char]]
lineChars =
    [ "┌┬┐↑"
    , "├┼┤│"
    , "└┴┘↓"
    , "←─→ "
    ]

lineDiagrams :: [[Diagram]]
lineDiagrams =
    [ [strokes [Right, Down], strokes [Left, Right, Down], strokes [Left, Down], arrow Up]
    ,
        [ strokes [Up, Right, Down]
        , strokes [Up, Right, Down, Left]
        , strokes [Up, Left, Down]
        , strokes [Up, Down]
        ]
    , [strokes [Up, Right], strokes [Up, Left, Right], strokes [Up, Left], arrow Down]
    , [arrow Left, strokes [Left, Right], arrow Right, []]
    ]
  where
    strokes = map (Stroke 0 . directionToPosition)
    arrow direction = [Arrow 0 direction, Stroke 0 (directionToNegPosition direction)]

lineCharMap :: Map.Map Char Diagram
lineCharMap =
    Map.map nub $
        Map.fromListWith
            (++)
            [ (char, diagram)
            | (chars, diagrams) <- zip lineChars lineDiagrams
            , (char, diagram) <- zip chars diagrams
            ]

charToDiagram :: Position -> Char -> Diagram
charToDiagram p c = case Map.lookup c lineCharMap of
    Just diagram -> map translate diagram
    Nothing -> [Text p [c]]
  where
    offset q = p + q
    translate element = case element of
        Stroke start end -> Stroke (offset start) (offset end)
        Arrow position direction -> Arrow (offset position) direction
        Text position content -> Text (offset position) content
