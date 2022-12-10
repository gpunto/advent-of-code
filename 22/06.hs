import Common (solveFile)
import Data.Set qualified as Set (fromList)

solve :: Int -> String -> Int
solve len = loop "" . zip [0 ..]
  where
    loop acc ((i, x) : xs)
      | length acc < len = loop (acc ++ [x]) xs
      | (len ==) . length . Set.fromList $ acc = i
      | otherwise = loop (tail acc ++ [x]) xs

s1 = solveFile (solve 4)

s2 = solveFile (solve 14)
