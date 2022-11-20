import Data.List (sort)
import Data.Maybe (catMaybes, isNothing, mapMaybe)

incomplete :: String -> Maybe String
incomplete = loop ""
  where
    loop stack [] = Just stack
    loop stack (c : cs)
      | isOpen c = loop (c : stack) cs
      | (not . null $ stack) && c `closes` head stack = loop (tail stack) cs
      | otherwise = Nothing

illegalChar :: String -> Maybe Char
illegalChar = loop ""
  where
    loop _ [] = Nothing
    loop stack (c : cs)
      | isOpen c = loop (c : stack) cs
      | (not . null $ stack) && c `closes` head stack = loop (tail stack) cs
      | otherwise = Just c

isOpen :: Char -> Bool
isOpen = (`elem` "([{<")

closes :: Char -> Char -> Bool
closes ')' '(' = True
closes ']' '[' = True
closes '}' '{' = True
closes '>' '<' = True
closes _ _ = False

close :: Char -> Char
close '(' = ')'
close '[' = ']'
close '{' = '}'
close '<' = '>'

illegalScore :: Char -> Int
illegalScore ')' = 3
illegalScore ']' = 57
illegalScore '}' = 1197
illegalScore '>' = 25137

completionScore :: String -> Int
completionScore = loop 0
  where
    loop acc [] = acc
    loop acc (c : cs) = loop (5 * acc + score c) cs
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

solve1 :: String -> Int
solve1 = sum . map illegalScore . mapMaybe illegalChar . lines

solve2 :: String -> Int
solve2 = middle . sort . map (completionScore . map close) . mapMaybe incomplete . lines

solveFile :: FilePath -> IO ()
solveFile fileName = print . solve2 =<< readFile fileName
