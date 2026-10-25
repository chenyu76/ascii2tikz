{-# LANGUAGE LambdaCase #-}

import Data.Char (isSpace)
import Data.List (foldl', intercalate, nub, partition, sortOn, transpose)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import System.Environment (getArgs)
import System.Exit (die)
import Prelude hiding (Left, Right)

main :: IO ()
main = do
    args <- getArgs
    options <- case args of
        [] -> pure ""
        [value] -> pure value
        _ -> die "Usage: ascii2tikz [TIKZ_OPTIONS]"
    interact
        (tikZToString options . simplifyTikZ . diagramToTikZ . diagramMatrixToDiagram . stringToDiagramMatrix)

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

-- PlusHalf n means n + 1/2, including when n is negative.
-- Multiplication and division fail when the exact result is not a half-integer.
data HalfInteger = Whole Integer | PlusHalf Integer
    deriving (Eq)

instance Show HalfInteger where
    show value =
        let units = halfUnits value
            (whole, half) = divMod (abs units) 2
         in (if units < 0 then "-" else "")
                ++ show whole
                ++ (if half == 0 then "" else ".5")

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
    deriving (Eq, Ord, Show)

-- Arithmetic is componentwise; scalar literals are broadcast to both components.
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

-- Positions are cell centers, with x increasing rightward and y downward.
-- Stroke stores an endpoint displacement; Text starts at its first character.
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

-- All points are absolute coordinates. Rectangle arguments are opposite corners.
-- Line and arrow paths need at least two points; shorter paths render nothing.
data TikZElement
    = TikZRectangle Vector2 Vector2
    | TikZLine [Vector2]
    | TikZArrow [Vector2]
    | TikZText Vector2 String
    deriving (Eq, Show)

type TikZ = [TikZElement]

-- Matrix conversion produces axis-aligned strokes directed rightward or downward.
-- Text moves from its first cell center to the top-left corner of that cell.
-- Arrow tips lie half a cell forward, with shafts starting half a cell behind.
diagramToTikZ :: PositionDiagram -> TikZ
diagramToTikZ = map convert
  where
    convert (PositionDiagramElement p item) = case item of
        Stroke delta -> TikZLine [p, p + delta]
        Arrow direction ->
            let halfStep = directionToVector2 direction / 2
             in TikZArrow [p - halfStep, p + halfStep]
        Text content -> TikZText (p - Vector2 (1 / 2) (1 / 2)) content

-- Rectangle recognition must precede polyline joining, which consumes straight edges.
simplifyTikZ :: TikZ -> TikZ
simplifyTikZ = simplifyPolylines . simplifyRectangles

-- Only two-point lines directed rightward or downward are rectangle candidates.
-- Matrix conversion already merges collinear strokes, so each side must be one line.
-- Find perpendicular sides sharing a start, then two sides ending at one corner.
-- Each line is consumed at most once; shared sides are not duplicated.
simplifyRectangles :: TikZ -> TikZ
simplifyRectangles diagram = Map.elems result
  where
    indexed = zip [0 :: Int ..] diagram
    segments = [(i, p, q) | (i, TikZLine [p, q]) <- indexed]
    starts = Map.fromListWith (++) [(p, [(i, q)]) | (i, p, q) <- segments]
    candidates =
        [ (top, left, right, bottom, p, s)
        | (top, p, q) <- segments
        , y p == y q
        , x p < x q
        , (left, r) <- Map.findWithDefault [] p starts
        , x p == x r
        , y p < y r
        , (right, s) <- Map.findWithDefault [] q starts
        , x s == x q
        , y s == y r
        , (bottom, t) <- Map.findWithDefault [] r starts
        , t == s
        ]
    (result, _) = foldl' replace (Map.fromList indexed, Map.fromList [(i, ()) | (i, _, _) <- segments]) candidates
    replace state@(items, available) (top, left, right, bottom, p, s)
        | all (`Map.member` available) edges =
            ( Map.insert top (TikZRectangle p s) (foldr Map.delete items edges)
            , foldr Map.delete available edges
            )
        | otherwise = state
      where
        edges = [top, left, right, bottom]

-- Only plain lines are joined; arrows, rectangles and text retain their meaning.
-- Connections use exact endpoint equality, without splitting at interior crossings.
-- Reverse paths as needed: canonical right/down strokes may meet at either end.
-- Join only endpoints incident to exactly two paths, leaving branch choices explicit.
simplifyPolylines :: TikZ -> TikZ
simplifyPolylines diagram = merge diagram
  where
    degrees =
        Map.fromListWith
            (+)
            [(p, 1 :: Int) | TikZLine points@(_ : _ : _) <- diagram, p <- [head points, last points]]
    merge [] = []
    merge (TikZLine points@(_ : _ : _) : rest) = extend points rest
    merge (item : rest) = item : merge rest
    extend points rest = case connected points rest of
        Just (joined, remaining) -> extend joined remaining
        Nothing -> TikZLine points : merge rest
    connected _ [] = Nothing
    connected points (item : rest) = case item of
        TikZLine next@(_ : _ : _) | Just joined <- join points next -> Just (joined, rest)
        _ -> case connected points rest of
            Just (joined, remaining) -> Just (joined, item : remaining)
            Nothing -> Nothing
    join points next
        | canJoin (last points) (head next) = Just (points ++ tail next)
        | canJoin (last points) (last next) = Just (points ++ tail (reverse next))
        | canJoin (head points) (last next) = Just (next ++ tail points)
        | canJoin (head points) (head next) = Just (reverse points ++ tail next)
        | otherwise = Nothing
    canJoin p q = p == q && Map.lookup p degrees == Just 2

-- The host document must load TikZ and its arrows.meta library.
-- The negative y scale preserves row order. Text keeps $ available for LaTeX math.
-- Extra options are raw TikZ keys appended after the defaults without validation.
-- Selecting a picture font measures M and its baseline spacing for the grid.
tikZToString :: String -> TikZ -> String
tikZToString options diagram =
    unlines $
        [ "\\begin{tikzpicture}["
        , "  font/.append code={#1\\tikzset{x={width(\"M\")},y=-\\baselineskip}},"
        , "  font=\\ttfamily,line cap=round,line join=round,"
        , "  diagram arrow/.style={-{Stealth[length=3pt,width=4pt]}},"
        , "  every node/.style={anchor=north west,align=left,inner sep=0pt,outer sep=0pt}"
            ++ (if null options then "" else ",")
        ]
            ++ [options | not (null options)]
            ++ ["]"]
            ++ map render diagram
            ++ ["\\end{tikzpicture}"]
  where
    coordinate (Vector2 px py) = "(" ++ show px ++ "," ++ show py ++ ")"
    render = \case
        TikZRectangle start end ->
            "\\draw " ++ coordinate start ++ " rectangle " ++ coordinate end ++ ";"
        TikZLine points -> renderPath "\\draw " points
        TikZArrow points -> renderPath "\\draw[diagram arrow] " points
        TikZText p content ->
            "\\node at "
                ++ coordinate p
                ++ " {\\strut "
                ++ concatMap escape content
                ++ "};"
    renderPath command points = case points of
        _ : _ : _ -> command ++ intercalate " -- " (map coordinate points) ++ ";"
        _ -> ""
    escape = \case
        -- I want to type some math.
        -- '\\' -> "\\textbackslash{}"
        -- '$' -> "\\$"
        -- '_' -> "\\_"
        -- '^' -> "\\textasciicircum{}"
        -- '{' -> "\\{"
        -- '}' -> "\\}"
        '&' -> "\\&"
        '#' -> "\\#"
        '%' -> "\\%"
        '~' -> "\\textasciitilde{}"
        ' ' -> "~"
        '\t' -> "\\quad{}"
        '\n' -> "\\\\{}\\strut "
        '\r' -> ""
        c -> [c]

-- Input is column-major, with each word stored only in its first occupied cell.
-- Remaining character cells and whitespace must be empty, as produced by the parser.
-- Row/column merging normalizes strokes to rightward/downward endpoint displacements.
-- Text joins across exactly one empty cell, then across consecutive rows at equal x.
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
                let end =
                        if along horizontal (q + nextDelta) > along horizontal (p + delta)
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

-- Entries must match lineChars in both row order and column order.
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
    -- Ordinary strokes reach halfway from the cell center toward a neighbor.
    -- Arrow conversion draws the full cell-width shaft; no additional Stroke is needed.
    strokes = map (Stroke . (/ 2) . directionToVector2)
    arrow direction = [Arrow direction]

lineCharMap :: Map.Map Char Diagram
lineCharMap =
    Map.map nub $
        Map.fromListWith
            (++)
            [ (char, diagram)
            | (chars, diagrams) <- zip lineChars lineDiagrams
            , (char, diagram) <- zip chars diagrams
            ]

-- Requires a character present in lineCharMap.
lineCharToDiagram :: Char -> Diagram
lineCharToDiagram c = lineCharMap Map.! c

-- Columns are the outer list, rows the inner list; shorter rows are padded with [].
-- Each Haskell Char occupies one cell; tabs and wide glyphs are not display-width expanded.
-- A word occupies one Text cell followed by empty placeholder cells for its other characters.
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
