import Common (splitOn)
import Data.List (groupBy, sortBy)

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . sortBy (flip compare) . map sum

parseInput :: String -> [[Int]]
parseInput = map (map read) . splitOn "" . lines

solveFile :: FilePath -> IO ()
solveFile filename = print . solve2 . parseInput =<< readFile filename
