{-# LANGUAGE LambdaCase #-}

{-
The center of a char
 1234
1┌──┐
2│  │
3└──┘
-}

import Data.Char (isSpace)
import Data.List (nub, partition, sortOn, transpose)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import Prelude hiding (Left, Right)

main :: IO ()
main = interact (diagramToTikZ . diagramMatrixToDiagram . stringToDiagramMatrix)

data Direction = Right | Up | Left | Down
    deriving (Eq, Show)

directionToNegVector2 :: Direction -> Vector2
directionToNegVector2 = directionToVector2 . oppositeDirection

oppositeDirection :: Direction -> Direction
oppositeDirection = \case
    Up -> Down
    Down -> Up
    Left -> Right
    Right -> Left

directionToVector2 :: Direction -> Vector2
directionToVector2 = \case
    Up -> Vector2 0 (-1)
    Down -> Vector2 0 1
    Left -> Vector2 (-1) 0
    Right -> Vector2 1 0

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
    deriving (Eq, Show)

type Diagram = [DiagramElement]

type PositionDiagram = [PositionDiagramElement]

diagramToTikZ :: PositionDiagram -> String
diagramToTikZ diagram = unlines $
    [ "\\documentclass[tikz,border=2pt]{standalone}"
    , "\\usepackage[UTF8,fontset=fandol]{ctex}"
    , "\\usetikzlibrary{arrows.meta}"
    , "\\begin{document}"
    , "\\begin{tikzpicture}[x=0.525em,y=-1.2em,font=\\ttfamily,line cap=round,line join=round]"
    ]
        ++ map render diagram
        ++ ["\\end{tikzpicture}", "\\end{document}"]
  where
    number value =
        let units = halfUnits value
            (whole, half) = divMod (abs units) 2
         in (if units < 0 then "-" else "")
                ++ show whole
                ++ (if half == 0 then "" else ".5")
    coordinate (Vector2 px py) = "(" ++ number px ++ "," ++ number py ++ ")"
    render (PositionDiagramElement p item) = case item of
        Stroke delta -> "\\draw " ++ coordinate p ++ " -- " ++ coordinate (p + delta) ++ ";"
        Arrow direction ->
            "\\draw[-{Stealth[length=3pt,width=4pt]}] "
                ++ coordinate (p - directionToVector2 direction / 2)
                ++ " -- " ++ coordinate p ++ ";"
        Text content ->
            "\\node[anchor=north west,align=left,inner sep=0pt,outer sep=0pt] at "
                ++ coordinate (p - Vector2 (1 / 2) (1 / 2))
                ++ " {\\strut " ++ concatMap escape content ++ "};"
    escape = \case
        '\\' -> "\\textbackslash{}"
        '{' -> "\\{"
        '}' -> "\\}"
        '$' -> "\\$"
        '&' -> "\\&"
        '#' -> "\\#"
        '%' -> "\\%"
        '_' -> "\\_"
        '^' -> "\\textasciicircum{}"
        '~' -> "\\textasciitilde{}"
        ' ' -> "~"
        '\t' -> "\\quad{}"
        '\n' -> "\\\\{}\\strut "
        '\r' -> ""
        c -> [c]

diagramMatrixToDiagram :: [[Diagram]] -> PositionDiagram
diagramMatrixToDiagram matrix =
    mergeTexts False (mergeStrokes False (mergeTexts True (mergeStrokes True positioned)))
  where
    cells =
        [ (Vector2 (fromIntegral column) (fromIntegral row), diagram)
        | (column, diagrams) <- zip [0 :: Int ..] matrix
        , (row, diagram) <- zip [0 :: Int ..] diagrams
        ]
    positioned =
        [ PositionDiagramElement p item
        | (p, diagram) <- cells
        , item <- diagram
        ]
    emptyCells = Map.fromList [((x p, y p), null diagram) | (p, diagram) <- cells]
    along horizontal = if horizontal then x else y
    across horizontal = if horizontal then y else x
    grouped horizontal items =
        Map.elems (Map.fromListWith (++) [(across horizontal (position item), [item]) | item <- items])
    mergeRuns _ [] = []
    mergeRuns _ [item] = [item]
    mergeRuns join (first : second : rest) = case join first second of
        Just merged -> mergeRuns join (merged : rest)
        Nothing -> first : mergeRuns join (second : rest)
    mergeStrokes horizontal items = remaining ++ concatMap mergeGroup (grouped horizontal strokes)
      where
        (strokes, remaining) = partition eligible items
        eligible (PositionDiagramElement _ (Stroke delta)) =
            across horizontal delta == 0 && along horizontal delta /= 0
        eligible _ = False
        normalize item@(PositionDiagramElement p (Stroke delta))
            | along horizontal delta < 0 = PositionDiagramElement (p + delta) (Stroke (negate delta))
            | otherwise = item
        normalize item = item
        mergeGroup = mergeRuns join . sortOn (along horizontal . position) . map normalize
        join (PositionDiagramElement p (Stroke delta)) (PositionDiagramElement q (Stroke nextDelta))
            | along horizontal q <= along horizontal (p + delta) =
                let end = if along horizontal (q + nextDelta) > along horizontal (p + delta)
                            then q + nextDelta
                            else p + delta
                 in Just (PositionDiagramElement p (Stroke (end - p)))
        join _ _ = Nothing
    mergeTexts horizontal items = remaining ++ concatMap mergeGroup (grouped horizontal texts)
      where
        (texts, remaining) = partition isText items
        isText (PositionDiagramElement _ (Text _)) = True
        isText _ = False
        mergeGroup = mergeRuns join . sortOn (along horizontal . position)
        join (PositionDiagramElement p (Text content)) (PositionDiagramElement q (Text nextContent))
            | horizontal
            , x q == x p + fromIntegral (length content) + 1
            , Map.lookup (x q - 1, y p) emptyCells == Just True =
                Just (PositionDiagramElement p (Text (content ++ " " ++ nextContent)))
            | not horizontal
            , y q == y p + fromIntegral (max 1 (length (lines content))) =
                Just (PositionDiagramElement p (Text (content ++ "\n" ++ nextContent)))
        join _ _ = Nothing

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
    arrow direction = [Arrow direction, Stroke (directionToNegVector2 direction / 2)]

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
