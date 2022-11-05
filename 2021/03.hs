import Control.Monad (liftM)
import Data.Char (digitToInt, intToDigit)

type Bit = Char

sumBit :: Int -> Bit -> Int
sumBit i '0' = i - 1
sumBit i '1' = i + 1

inverse :: Int -> Int
inverse 0 = 1
inverse 1 = 0

sumBinary :: [Int] -> [Bit] -> [Int]
sumBinary = zipWith sumBit

toDecimal :: [Int] -> Int
toDecimal = loop 0 1 . reverse
  where
    loop acc _ [] = acc
    loop acc factor (d : ds) = loop (acc + d * factor) (factor * 2) ds

intBit :: (Int -> Int -> Bool) -> (Int -> Int)
intBit cmp n = if n `cmp` 0 then 1 else 0

rate :: (Int -> Int -> Bool) -> [Int] -> Int
rate cmp = toDecimal . map (intBit cmp)

solve1 :: String -> Int
solve1 input = rate (>) bitCount * rate (<) bitCount
  where
    bitCount = foldl sumBinary (repeat 0) . lines $ input

only :: Bit -> Int -> [[Bit]] -> [[Bit]]
only bit pos = filter (\w -> w !! pos == bit)

sumNthBit :: Int -> [[Bit]] -> Int
sumNthBit n = foldl (\acc w -> sumBit acc (w !! n)) 0

rating :: (Int -> Int) -> Int -> [[Bit]] -> Int
rating _ _ [w] = toDecimal . map digitToInt $ w
rating f n ws = rating f (n + 1) remaining
  where
    common = intToDigit . f . intBit (>=) . sumNthBit n $ ws
    remaining = only common n ws

solve2 :: String -> Int
solve2 input = rating inverse 0 bits * rating id 0 bits
  where
    bits = lines input

solveFile :: (String -> Int) -> FilePath -> IO ()
solveFile f fileName = print . f =<< readFile fileName