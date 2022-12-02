import Common (parseCSLine)

solve :: [Int] -> Int
solve cs = minimum $ map fuelCost2 [minimum cs .. maximum cs]
  where
    fuelCost1 c = sum . map (\x -> abs (x - c)) $ cs
    fuelCost2 c = sum . map (\x ->
        let n = abs (x - c)
         in n * (n + 1) `div` 2
      ) $ cs

solveFile :: FilePath -> IO ()
solveFile filename = print . solve . parseCSLine =<< readFile filename
