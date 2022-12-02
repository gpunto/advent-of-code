import Data.Array (Array, Ix (inRange), assocs, bounds, listArray, (!))
import Data.Char (digitToInt)
import Data.List (sort, sortBy)
import Data.Set qualified as Set

type Matrix = Array Int (Array Int Int)

parseInput :: String -> Matrix
parseInput input = listArray (0, rows - 1) . map toIntArray $ ls
  where
    ls = lines input
    rows = length ls
    cols = length . head $ ls
    toIntArray = listArray (0, cols - 1) . map digitToInt

solve1 :: Matrix -> Int
solve1 = sum . map ((1 +) . snd) . filterLow

filterLow :: Matrix -> [(Int, Int)]
filterLow m =
  filter (isLow m)
    . concatMap (\(r, arr) -> map (\(c, n) -> (r, c)) . assocs $ arr)
    . assocs
    $ m

isLow :: Matrix -> (Int, Int) -> Bool
isLow m (r, c) = matrixGet m (r, c) < minimum (map (matrixGet m) $ adjacent r c m)

adjacent :: Int -> Int -> Matrix -> [(Int, Int)]
adjacent r c m =
  filter
    (\(a, b) -> inRange (0, maxRow) a && inRange (0, maxCol) b)
    [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]
  where
    maxRow = snd . bounds $ m
    maxCol = snd . bounds $ m ! 0

matrixGet :: Matrix -> (Int, Int) -> Int
matrixGet m (r, c) = (m ! r) ! c

solve2 :: Matrix -> Int
solve2 m = product . take 3 . sortBy (flip compare) . map (basinSize m) . filterLow $ m

basinSize :: Matrix -> (Int, Int) -> Int
basinSize m p = Set.size . loop (Set.singleton p) $ p
  where
    loop :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
    loop acc (r, c) =
      if null adjs
        then acc
        else Set.unions . map (loop newAcc) $ adjs
      where
        newAcc = Set.union acc . Set.fromList $ adjs
        adjs =
          filter isHigherPoint
            . filter (`Set.notMember` acc)
            $ adjacent r c m
        isHigherPoint p1 =
          let value = matrixGet m p1
           in value < 9 && matrixGet m (r, c) < value

solveFile :: FilePath -> IO ()
solveFile fileName = print . solve2 . parseInput =<< readFile fileName
