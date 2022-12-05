import Common (solveFile)
import Data.Map qualified as Map

data Move = Rock | Paper | Scissors deriving (Show, Enum)

data Result = Win | Lose | Draw deriving (Show)

moveParseMap :: Map.Map Char Move
moveParseMap =
  Map.fromList
    [ ('A', Rock),
      ('X', Rock),
      ('B', Paper),
      ('Y', Paper),
      ('C', Scissors),
      ('Z', Scissors)
    ]

resultParseMap :: Map.Map Char Result
resultParseMap =
  Map.fromList
    [ ('X', Lose),
      ('Y', Draw),
      ('Z', Win)
    ]

parse1 :: String -> [(Move, Move)]
parse1 = map parse . lines
  where
    parse [x, ' ', y] = (moveParseMap Map.! x, moveParseMap Map.! y)

solve1 :: [(Move, Move)] -> Int
solve1 = sum . map score

score :: (Move, Move) -> Int
score (a, b) = shapeScore b + winnerScore a b
  where
    shapeScore = (1 +) . fromEnum
    winnerScore Rock Paper = 6
    winnerScore Rock Scissors = 0
    winnerScore Paper Rock = 0
    winnerScore Paper Scissors = 6
    winnerScore Scissors Rock = 6
    winnerScore Scissors Paper = 0
    winnerScore _ _ = 3

parse2 :: String -> [(Move, Result)]
parse2 = map parse . lines
  where
    parse [x, ' ', y] = (moveParseMap Map.! x, resultParseMap Map.! y)

prev :: Move -> Move
prev Rock = Scissors
prev x = pred x

next :: Move -> Move
next Scissors = Rock
next x = succ x

solve2 :: [(Move, Result)] -> Int
solve2 = sum . map (score . playFor)
  where
    playFor :: (Move, Result) -> (Move, Move)
    playFor (m, Draw) = (m, m)
    playFor (m, Win) = (m, next m)
    playFor (m, Lose) = (m, prev m)

s1 = solveFile $ solve1 . parse1

s2 = solveFile $ solve2 . parse2
