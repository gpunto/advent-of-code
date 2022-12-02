import Data.Array.IArray
import Common (parseCSLine)

newtype Lanternfish = Lanternfish Int deriving (Show, Eq)

type Population = Array Int Int

adult :: Lanternfish
adult = Lanternfish 6

newborn :: Lanternfish
newborn = Lanternfish 8

ageFish :: Lanternfish -> [Lanternfish]
ageFish (Lanternfish 0) = [adult, newborn]
ageFish (Lanternfish d) = [Lanternfish (d - 1)]

agePopulation :: Int -> [Lanternfish] -> [Lanternfish]
agePopulation 0 p = p
agePopulation days p = agePopulation (days - 1) . concatMap ageFish $ p

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = loop []
  where
    loop acc [] = reverse acc
    loop acc l =
      let (before, after) = span (/= d) l
       in loop (before : acc) (drop 1 after)

solveNaive :: Int -> FilePath -> IO ()
solveNaive days filename =
  print
    . length
    . agePopulation days
    . map Lanternfish
    . parseCSLine
    =<< readFile filename

emptyPopulation :: Population
emptyPopulation = listArray (0, 8) (repeat 0)

loadPopulation :: [Int] -> Population
loadPopulation = foldr accumulate emptyPopulation
  where
    accumulate i p = accum (+) p [(i, 1)]

age :: Population -> Population
age p = ixmap (0, 8) newIndex p // [(6, p ! 7 + p ! 0)]
  where
    newIndex i = (i + 1) `mod` 9

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ a = a
nTimes n f a = nTimes (n - 1) f (f a)

solve :: Int -> FilePath -> IO ()
solve days filename =
  print
    . sum
    . nTimes days age
    . loadPopulation
    . parseCSLine
    =<< readFile filename
