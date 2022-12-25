import Common (solveFile, splitOn)
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Map qualified as M

data FileTree = Empty | Directory String [FileTree] | File String Integer deriving (Show)

data Log = Cd String | Ls [LsEntry] deriving (Show)

data LsEntry = Dir String | FileSize String Integer deriving (Show)

parseLogLine :: String -> Log
parseLogLine s = case s of
  ('c' : 'd' : ' ' : ss) -> Cd ss
  ('l' : 's' : ss) -> Ls (map parseLsEntry . lines . tail $ ss)

parseLsEntry :: String -> LsEntry
parseLsEntry s =
  let (a, b) = tail <$> break isSpace s
   in if a == "dir"
        then Dir b
        else FileSize b (read a)

parseInput :: String -> [Log]
parseInput = map (parseLogLine . trim) . tail . splitOn '$'

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

buildTree :: [Log] -> FileTree
buildTree = snd . foldl' loop ([], Empty)
  where
    loop (path, ft) (Cd "..") = (init path, ft)
    loop (path, ft) (Cd name) = (path ++ [name], insert (Directory name []) path ft)
    loop (path, ft) (Ls entries) = (path, foldr (insertEntry path) ft entries)

insertEntry :: [String] -> LsEntry -> FileTree -> FileTree
insertEntry path entry to = case entry of
  (Dir _) -> to
  (FileSize name size) -> insert (File name size) path to

insert :: FileTree -> [String] -> FileTree -> FileTree
insert ft [] Empty = ft
insert _ [] to = to
insert _ _ to@(File _ _) = to
insert ft [p] (Directory n ch) = Directory n $ if p == n then ft : ch else ch
insert ft (p : ps) (Directory n ch) =
  Directory n $
    if p == n
      then map (insert ft ps) ch
      else ch

sizes :: FileTree -> M.Map [String] Integer
sizes = go M.empty []
  where
    go :: M.Map [String] Integer -> [String] -> FileTree -> M.Map [String] Integer
    go m path d@(Directory name fts) =
      let newPath = name : path
          newM = M.insert newPath (totalSize d) m
       in foldl' (`go` newPath) newM fts
    go m path _ = m

totalSize :: FileTree -> Integer
totalSize Empty = 0
totalSize (File _ size) = size
totalSize (Directory name fts) = sum . map totalSize $ fts

required :: Integer
required = 30000000

total :: Integer
total = 70000000

solve1 :: String -> Integer
solve1 = sum . M.filter (< 100000) . sizes . buildTree . parseInput

solve2 :: String -> Integer
solve2 s = minimum . M.filter (> toFree) . sizes $ ft
  where
    ft = buildTree . parseInput $ s
    toFree = required - (total - totalSize ft)

s1 = solveFile solve1

s2 = solveFile solve2
