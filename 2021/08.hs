import Data.Char (intToDigit)
import Data.List (sort)
import Data.Map.Strict qualified as Map

type Line = ([String], [String])

isSimpleDigit :: String -> Bool
isSimpleDigit s = let l = length s in l `elem` [2, 3, 4, 7]

parseLine :: String -> Line
parseLine s = (patterns, output)
  where
    ws = words s
    (patterns, rest) = span (/= "|") ws
    output = tail rest

parseInput :: String -> [Line]
parseInput = map parseLine . lines

solve1 :: [Line] -> Int
solve1 = sum . map countSimple
  where
    countSimple = length . filter isSimpleDigit . snd

decodeLine :: Line -> Int
decodeLine (signals, output) = read . map (intToDigit . decodeDigit mapping) $ output
  where
    mapping =
      Map.fromList
        [ (a, 'a'),
          (b, 'b'),
          (c, 'c'),
          (d, 'd'),
          (e, 'e'),
          (f, 'f'),
          (g, 'g')
        ]
    occ = occurrences signals
    b = keyForValue 6 occ
    e = keyForValue 4 occ
    f = keyForValue 9 occ
    c = findChar [f] 2
    a = findChar [c, f] 3
    d = findChar [b, c, f] 4
    g = findChar [a, b, c, d, e, f] 7
    findChar excl len = head . filter (`notElem` excl) . head . filter ((len ==) . length) $ signals

decodeDigit :: Map.Map Char Char -> String -> Int
decodeDigit m = (digitMapping Map.!) . sort . map (m Map.!)

digitMapping :: Map.Map String Int
digitMapping =
  Map.fromList
    [ ("abcefg", 0),
      ("cf", 1),
      ("acdeg", 2),
      ("acdfg", 3),
      ("bcdf", 4),
      ("abdfg", 5),
      ("abdefg", 6),
      ("acf", 7),
      ("abcdefg", 8),
      ("abcdfg", 9)
    ]

keyForValue :: Eq b => b -> Map.Map a b -> a
keyForValue v = fst . head . filter ((== v) . snd) . Map.assocs

occurrences :: [String] -> Map.Map Char Int
occurrences = foldl accum Map.empty . concat
  where
    accum m c = Map.insertWith (+) c 1 m

solve2 :: [Line] -> Int
solve2 = sum . map decodeLine

solveFile :: FilePath -> IO ()
solveFile filename = print . solve2 . parseInput =<< readFile filename
