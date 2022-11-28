solve1 :: String -> Int
solve1 = sum . map toDelta

toDelta :: Char -> Int
toDelta '(' = 1
toDelta ')' = -1
toDelta _ = 0

solve2 :: String -> Int
solve2 = fst . head . dropWhile ((-1 /=) . snd) . zip [1 ..] . drop 1 . scanl (+) 0 . map toDelta

solveFile :: FilePath -> IO ()
solveFile filename = print . solve2 =<< readFile filename
