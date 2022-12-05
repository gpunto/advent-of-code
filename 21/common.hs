module Common where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = loop []
  where
    loop acc [] = reverse acc
    loop acc l =
      let (before, after) = span (/= d) l
       in loop (before : acc) (drop 1 after)

parseCSLine :: Read a => String -> [a]
parseCSLine = map read . splitOn ','

solveFile :: Show a => (String -> a) -> FilePath -> IO ()
solveFile solve filename = print . solve =<< readFile filename
