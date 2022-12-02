import Data.Char (isDigit, isSpace)
import Data.List (group, sort)

type Point = (Int, Int)

rangeToLine :: Point -> Point -> [Point]
rangeToLine p1 p2
  | isDiagonal p1 p2 = diagonalLine p1 p2
  | otherwise = rectLine p1 p2

rectLine :: Point -> Point -> [Point]
rectLine p1 p2 = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
  where
    (x1, y1) = min p1 p2
    (x2, y2) = max p1 p2

diagonalLine :: Point -> Point -> [Point]
diagonalLine (x1, y1) (x2, y2) = [(opS i, opE i) | i <- [0 .. (abs (x1 - x2))]]
  where
    opS = if x1 < x2 then (x1 +) else (x1 -)
    opE = if y1 < y2 then (y1 +) else (y1 -)

isDiagonal :: Point -> Point -> Bool
isDiagonal (x1, y1) (x2, y2) = x1 /= x2 && y1 /= y2

parsePoint :: String -> Point
parsePoint s = (read x, read y)
  where
    (x, rest) = break (== ',') s
    y = tail rest

parseRange :: String -> (Point, Point)
parseRange line = (parsePoint start, parsePoint end)
  where
    (start, rest) = break isSpace line
    (_, end) = break isDigit rest

parseInput :: String -> [Point]
parseInput =
  concatMap (uncurry rangeToLine)
    -- . filter (not . uncurry isDiagonal)
    . map parseRange
    . lines

solve :: String -> Int
solve = length . map head . filter (\ps -> length ps > 1) . group . sort . parseInput

solveFile :: FilePath -> IO ()
solveFile filename = print . solve =<< readFile filename
