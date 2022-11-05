import Data.Char
import Debug.Trace (trace)

countLarger :: [Integer] -> Integer
countLarger [] = 0
countLarger (x : xs) = loop 0 x xs
  where
    loop count _ [] = count
    loop count prev (x : xs)
      | x > prev = loop (count + 1) x xs
      | otherwise = loop count x xs

chunk' :: Int -> [a] -> [[a]]
chunk' n = loop []
  where
    loop acc [] = reverse acc
    loop acc xs = loop (take n xs : acc) (tail xs)

countSumLarger :: [Integer] -> Integer
countSumLarger = countLarger . map sum . filter ((3 ==) . length) . chunk' 3

solve1 = solve countLarger

solve2 = solve countSumLarger

solve f = show . f . map read . lines
