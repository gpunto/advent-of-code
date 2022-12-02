import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . requirements . lines $ contents

requirements :: [String] -> Int
requirements = sum . map (fuel2 . toInt)
    where toInt s = read s :: Int

fuel :: Int -> Int
fuel mass = div mass 3 - 2

fuel2 :: Int -> Int
fuel2 = flip fuel2co 0

fuel2co :: Int -> Int -> Int
fuel2co mass acc
    | toAdd <= 0 = acc
    | otherwise = fuel2co toAdd (toAdd + acc)
    where toAdd = fuel mass