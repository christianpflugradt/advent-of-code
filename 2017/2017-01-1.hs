import Data.Char (digitToInt)
import System.IO (readFile)

iterateAndPeek :: [Int] -> [(Int, Maybe Int)]
iterateAndPeek [] = []
iterateAndPeek [x] = [(x, Nothing)]
iterateAndPeek [x, y] = [(x, Just y)]
iterateAndPeek (x:y:xs) = (x, Just y) : iterateAndPeek (y:xs)

fold :: [(Int, Maybe Int)] -> Int
fold [] = 0
fold (x:xs) = digitPairVal x + fold xs

digitPairVal :: (Int, Maybe Int) -> Int
digitPairVal (x, Just y) = if x == y then x else 0
digitPairVal (x, Nothing) = 0

addFirstToBack :: [a] -> [a]
addFirstToBack [] = []
addFirstToBack [x] = [x]
addFirstToBack (x:xs) = [x] ++ xs ++ [x]

solve :: String -> Int
solve = fold . iterateAndPeek . addFirstToBack . map digitToInt

main :: IO ()
main = print . solve =<< readFile "2017-01.txt"
