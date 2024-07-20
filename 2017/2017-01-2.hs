import Data.Char (digitToInt)
import System.IO (readFile)

iterateAndPeek :: [Int] -> [(Int, Maybe Int)]
iterateAndPeek [] = []
iterateAndPeek [x] = [(x, Nothing)]
iterateAndPeek [x, y] = [(x, Just y), (y, Just x)]
iterateAndPeek xs = zip xs peeked
  where
    len = length xs
    halfWay = len `div` 2
    rotated = take len (drop halfWay (cycle xs))
    peeked = map Just rotated

fold :: [(Int, Maybe Int)] -> Int
fold [] = 0
fold (x:xs) = digitPairVal x + fold xs

digitPairVal :: (Int, Maybe Int) -> Int
digitPairVal (x, Just y) = if x == y then x else 0
digitPairVal (x, Nothing) = 0

solve :: String -> Int
solve = fold . iterateAndPeek . map digitToInt

main :: IO ()
main = print . solve =<< readFile "2017-01.txt"
