import Data.Array
import Data.List.Split (splitOn)

type Memory = Array Int Int

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . part2 $ parseToArray contents

part1 :: Memory -> Int -> Int -> Int
part1 memory i j = head . run $ memory // [(1, i), (2, j)]

part2 :: Memory -> Int
part2 memory = noun * 100 + verb
    where (noun, verb) = head [ (i, j) | i <- [0..99], j <- [0..99], 19690720 == part1 memory i j]

run :: Memory -> [Int]
run memory = elems $ execute memory 0

parseToArray :: String -> Memory
parseToArray input =
    listArray (0, length l - 1) l
        where
            l = map toInt (splitOn "," input)
            toInt s = read s :: Int

execute :: Memory -> Int -> Memory
execute arr pos =
    case arr!pos of
        1 -> execute (add arr pos) (pos + 4)
        2 -> execute (mul arr pos) (pos + 4)
        99 -> arr

add :: Memory -> Int -> Memory
add = perform (+)

mul :: Memory -> Int -> Memory
mul = perform (*)

perform :: (Int -> Int -> Int) -> Memory -> Int -> Memory
perform op arr cmdPos = arr // [(at 3, op (at2 1) (at2 2))]
    where
        at n = arr ! (cmdPos + n)
        at2 n = arr ! at n