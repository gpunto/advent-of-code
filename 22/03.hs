import Common (chunksOf, solveFile)
import Data.Char (isLower, isUpper, ord)

solve1 :: String -> Int
solve1 = sum . map (priority . inBoth) . lines
  where
    inBoth s = (\(h1, h2) -> findSame h1 [h2]) $ splitAt (length s `div` 2) s

findSame :: String -> [String] -> Char
findSame (h : hs) h2s
  | all (h `elem`) h2s = h
  | otherwise = findSame hs h2s

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27

solve2 :: String -> Int
solve2 = sum . map (priority . findBadge) . chunksOf 3 . lines
  where
    findBadge [s1, s2, s3] = findSame s1 [s2, s3]

s1 = solveFile solve1

s2 = solveFile solve2
