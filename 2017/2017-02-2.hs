import System.IO (readFile)
import AocCommon (splitByChar, splitLines)

mapNumbers :: String -> [[Int]]
mapNumbers = map (map read . splitByChar '\t') . splitLines

divisiblePairs :: [Int] -> [(Int, Int)]
divisiblePairs xs = [(a, b) | a <- xs, b <- xs, a/=b, a `mod` b == 0]

evenDivision :: [Int] -> Int
evenDivision xs = fst pair `div` snd pair
    where
        pair = head $ divisiblePairs xs

fold :: [[Int]] -> Int
fold = sum . map evenDivision

solve :: String -> Int
solve = fold . mapNumbers

main :: IO ()
main = print . solve =<< readFile "2017-02.txt"
