import Common (chunksOf, solveFile)
import Data.Array.IArray qualified as Arr
import Data.Char (isDigit)
import Debug.Trace (trace)

data Move = Move {quantity :: Int, from :: Int, to :: Int} deriving (Show)

type Stacks = Arr.Array Int [Char]

parseInput :: String -> (Stacks, [Move])
parseInput input = (parseStacks ss, parseMoves ms)
  where
    (ss, _ : ms) = span (/= "") . lines $ input

parseStacks :: [String] -> Stacks
parseStacks lines =
  Arr.amap init
    . Arr.accumArray (++) "" (1, len)
    . concatMap (zip [1 ..])
    $ rows
  where
    clean = filter (not . flip elem [' ', '[', ']'])
    rows = map (map clean . chunksOf 4) lines
    len = length . head $ rows

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

parseMove :: String -> Move
parseMove input = Move (read qs) (read fs) (read ts)
  where
    (qs, r1) = span (/= ' ') . drop 5 $ input
    (fs, r2) = span (/= ' ') . drop 6 $ r1
    ts = drop 4 r2

solve :: (String -> String) -> (Stacks, [Move]) -> String
solve order (stacks, moves) = concatMap (take 1) . Arr.elems . foldl acc stacks $ moves
  where
    acc stacks (Move q f t) =
      let (taken, remaining) = splitAt q $ stacks Arr.! f
       in Arr.accum (flip (++)) stacks [(t, order taken)] Arr.// [(f, remaining)]

s1 = solveFile (solve reverse . parseInput)

s2 = solveFile (solve id . parseInput)
