import Common (chunksOf)
import Data.Char

countLarger :: [Integer] -> Integer
countLarger [] = 0
countLarger (x : xs) = loop 0 x xs
  where
    loop count _ [] = count
    loop count prev (x : xs)
      | x > prev = loop (count + 1) x xs
      | otherwise = loop count x xs

countSumLarger :: [Integer] -> Integer
countSumLarger = countLarger . map sum . filter ((3 ==) . length) . chunksOf 3

solve1 = solve countLarger

solve2 = solve countSumLarger

solve f = show . f . map read . lines
