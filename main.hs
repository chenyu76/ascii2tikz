{-# LANGUAGE LambdaCase #-}

import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Prelude hiding (Left, Right)

data Direction = Right | Up | Left | Down
    deriving (Eq, Show)

directionToPosition :: Direction -> Position
directionToPosition = \case
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

data Position = Position
    { x :: Int
    , y :: Int
    }
    deriving (Eq, Show)

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

lineChars :: [[String]]
lineChars =
    [
        [ "┌┬┐↑"
        , "├┼┤│"
        , "└┴┘↓"
        , "←─→ "
        ]
    ,
        [ "╔╦╗▲"
        , "╠╬╣║"
        , "╚╩╝▼"
        , "◄═► "
        ]
    ,
        [ "+++^"
        , "+++|"
        , "+++v"
        , "<-> "
        ]
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
    origin = Position 0 0
    strokes = map (Stroke origin . directionToPosition)
    arrow direction = [Arrow origin direction, Stroke origin (directionToPosition (oppositeDirection direction ))]

lineCharMap :: Map.Map Char Diagram
lineCharMap =
    Map.map nub $
        Map.fromListWith
            (++)
            [ (char, diagram)
            | style <- lineChars
            , (chars, diagrams) <- zip style lineDiagrams
            , (char, diagram) <- zip chars diagrams
            ]

charToDiagram :: Position -> Char -> Diagram
charToDiagram p c
    | isSpace c = []
    | otherwise = case Map.lookup c lineCharMap of
        Just diagram -> map translate diagram
        Nothing -> [Text p [c]]
  where
    offset q = Position (x p + x q) (y p + y q)
    translate element = case element of
        Stroke start end -> Stroke (offset start) (offset end)
        Arrow position direction -> Arrow (offset position) direction
        Text position content -> Text (offset position) content
