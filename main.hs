{-# LANGUAGE LambdaCase #-}

{-
The center of a char
 1234
1┌──┐
2│  │
3└──┘
-}

import Data.Char (isSpace)
import Data.List (nub, transpose)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import Prelude hiding (Left, Right)

data Direction = Right | Up | Left | Down
    deriving (Eq, Show)

directionToNegVector2 :: Direction -> Vector2
directionToNegVector2 = \case
    Up -> Vector2 0 (-1)
    Down -> Vector2 0 1
    Left -> Vector2 (-1) 0
    Right -> Vector2 1 0

oppositeDirection :: Direction -> Direction
oppositeDirection = \case
    Up -> Down
    Down -> Up
    Left -> Right
    Right -> Left

directionToVector2 :: Direction -> Vector2
directionToVector2 = directionToNegVector2 . oppositeDirection

data HalfInteger = Whole Integer | PlusHalf Integer
    deriving (Eq, Show)

halfUnits :: HalfInteger -> Integer
halfUnits (Whole n) = 2 * n
halfUnits (PlusHalf n) = 2 * n + 1

fromHalfUnits :: Integer -> HalfInteger
fromHalfUnits n = case divMod n 2 of
    (whole, 0) -> Whole whole
    (whole, _) -> PlusHalf whole

instance Ord HalfInteger where
    compare a b = compare (halfUnits a) (halfUnits b)

instance Num HalfInteger where
    a + b = fromHalfUnits (halfUnits a + halfUnits b)
    a - b = fromHalfUnits (halfUnits a - halfUnits b)
    a * b = case divMod (halfUnits a * halfUnits b) 2 of
        (units, 0) -> fromHalfUnits units
        _ -> error "HalfInteger multiplication result is not an integer or half-integer"
    negate = fromHalfUnits . negate . halfUnits
    abs = fromHalfUnits . abs . halfUnits
    signum = Whole . signum . halfUnits
    fromInteger = Whole

instance Fractional HalfInteger where
    a / b
        | halfUnits b == 0 = error "HalfInteger division by zero"
        | otherwise = case divMod (2 * halfUnits a) (halfUnits b) of
            (units, 0) -> fromHalfUnits units
            _ -> error "HalfInteger division result is not an integer or half-integer"
    fromRational value = case divMod (2 * numerator value) (denominator value) of
        (units, 0) -> fromHalfUnits units
        _ -> error "HalfInteger rational value is not an integer or half-integer"

data Vector2 = Vector2
    { x :: HalfInteger
    , y :: HalfInteger
    }
    deriving (Eq, Show)

instance Num Vector2 where
    Vector2 x1 y1 + Vector2 x2 y2 = Vector2 (x1 + x2) (y1 + y2)
    Vector2 x1 y1 - Vector2 x2 y2 = Vector2 (x1 - x2) (y1 - y2)
    Vector2 x1 y1 * Vector2 x2 y2 = Vector2 (x1 * x2) (y1 * y2)
    negate (Vector2 px py) = Vector2 (negate px) (negate py)
    abs (Vector2 px py) = Vector2 (abs px) (abs py)
    signum (Vector2 px py) = Vector2 (signum px) (signum py)
    fromInteger n = Vector2 (fromInteger n) (fromInteger n)

instance Fractional Vector2 where
    Vector2 x1 y1 / Vector2 x2 y2 = Vector2 (x1 / x2) (y1 / y2)
    fromRational n = Vector2 (fromRational n) (fromRational n)

data DiagramElement
    = Stroke Vector2
    | Arrow Direction
    | Text String
    deriving (Eq, Show)

data PositionDiagramElement = PositionDiagramElement
    { position :: Vector2
    , element :: DiagramElement
    }

type Diagram = [DiagramElement]

type PositionDiagram = [PositionDiagramElement]

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
    strokes = map (Stroke . (/ 2) . directionToVector2)
    arrow direction = [Arrow direction, Stroke (directionToNegVector2 direction)]

lineCharMap :: Map.Map Char Diagram
lineCharMap =
    Map.map nub $
        Map.fromListWith
            (++)
            [ (char, diagram)
            | (chars, diagrams) <- zip lineChars lineDiagrams
            , (char, diagram) <- zip chars diagrams
            ]

lineCharToDiagram :: Char -> Diagram
lineCharToDiagram c = lineCharMap Map.! c

stringToDiagramMatrix :: String -> [[Diagram]]
stringToDiagramMatrix str = transpose (map (parse 0) rows)
  where
    rows = lines str
    width = maximum (0 : map length rows)
    parse column [] = replicate (width - column) mempty
    parse column (c : rest)
        | isSpace c = mempty : parse (column + 1) rest
        | otherwise = case Map.lookup c lineCharMap of
            Just l -> l : parse (column + 1) rest
            Nothing ->
                let (word, remaining) = span (\ch -> not (isSpace ch || isLineChar ch)) (c : rest)
                    wordLength = length word
                 in [Text word] : replicate (wordLength - 1) mempty ++ parse (column + wordLength) remaining

    isLineChar :: Char -> Bool
    isLineChar c = case Map.lookup c lineCharMap of
        Just _ -> True
        Nothing -> False
