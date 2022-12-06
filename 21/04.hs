import Common (chunksOf)
import Data.List (transpose)

type Board = [[Cell]]

data Cell = Cell {number :: Int, marked :: Bool} deriving (Show)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = loop []
  where
    loop acc [] = reverse acc
    loop acc l =
      let (before, after) = span (/= d) l
       in loop (before : acc) (drop 1 after)

parseExtractions :: String -> [Int]
parseExtractions = map read . splitOn ','

isWinning :: Board -> Bool
isWinning rows = any isWinningLine (rows ++ cols)
  where
    cols = transpose rows
    isWinningLine = all marked

mark :: Int -> Board -> Board
mark e = map (map markCell)
  where
    markCell (Cell n m) = Cell n (m || e == n)

unmarkedSum :: Board -> Int
unmarkedSum = sum . map (sum . map value)
  where
    value (Cell n m) = if m then 0 else n

parseBoards :: [String] -> [Board]
parseBoards = chunksOf 5 . chunksOf 5 . map ((`Cell` False) . read) . concatMap words

parseInput :: String -> ([Int], [Board])
parseInput input = (parseExtractions firstLine, boards)
  where
    (firstLine : rest) = lines input
    boards = parseBoards . filter (not . null) $ rest

solve1 :: [Int] -> [Board] -> Int
solve1 (e : es) boards =
  let marked = map (mark e) boards
   in case filter isWinning marked of
        (wb : _) -> unmarkedSum wb * e
        [] -> solve1 es marked

solve2 :: [Int] -> [Board] -> Int
solve2 extractions boards = loop extractions boards 0
  where
    loop (e : es) boards score =
      let marked = map (mark e) boards
       in case filter isWinning marked of
            (_ : _) -> loop es (filter (not . isWinning) marked) (unmarkedSum (last marked) * e)
            [] -> loop es marked score
    loop [] _ score = score

solveFile :: ([Int] -> [Board] -> Int) -> FilePath -> IO ()
solveFile solve fileName = print . uncurry solve . parseInput =<< readFile fileName
