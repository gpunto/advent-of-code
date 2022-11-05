import Data.Char (isSpace)
import Data.List (groupBy)

data Command = Command String Int deriving (Show)

data Position = Position {x :: Int, depth :: Int, aim :: Int} deriving (Show)

parseCommand :: String -> Command
parseCommand s = Command direction (read distance)
  where
    (direction, distance) = break isSpace s

executeCommand1 :: Position -> Command -> Position
executeCommand1 p (Command direction amount) = case direction of
  "forward" -> p {x = x p + amount}
  "up" -> p {depth = depth p - amount}
  "down" -> p {depth = depth p + amount}

executeCommand2 :: Position -> Command -> Position
executeCommand2 (Position x depth aim) (Command direction amount) = case direction of
  "forward" -> Position (x + amount) (depth + aim * amount) aim
  "up" -> Position x depth (aim - amount)
  "down" -> Position x depth (aim + amount)

solve :: (Position -> Command -> Position) -> FilePath -> IO ()
solve execute fileName =
  print
    . (\(Position x y _) -> x * y)
    . foldl execute (Position 0 0 0)
    . map parseCommand
    . lines
    =<< readFile fileName
