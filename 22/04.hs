import Common (solveFile)

type Range = (Int, Int)

solve1 :: [(Range, Range)] -> Int
solve1 = length . filter (uncurry anyContained)

anyContained :: Range -> Range -> Bool
anyContained r1 r2 = isContained r1 r2 || isContained r2 r1
  where
    isContained (s1, e1) (s2, e2) = s1 >= s2 && e1 <= e2

solve2 :: [(Range, Range)] -> Int
solve2 = length . filter (uncurry overlap)

overlap :: Range -> Range -> Bool
overlap r1@(s1, e1) r2@(s2, e2) = between s1 r2 || between e1 r2 || between s2 r1 || between e2 r1
  where
    between a (s, e) = a >= s && a <= e

parseInput :: String -> [(Range, Range)]
parseInput = map parsePairs . lines
  where
    parsePairs = both parseRange . spanExcluding ','
    parseRange = both read . spanExcluding '-'

spanExcluding :: Eq a => a -> [a] -> ([a], [a])
spanExcluding x = tailSnd . span (/= x)
  where
    tailSnd (a, []) = (a, [])
    tailSnd (a, b) = (a, tail b)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

s1 = solveFile (solve1 . parseInput)

s2 = solveFile (solve2 . parseInput)
