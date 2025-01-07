import System.IO (readFile)
import AocCommon (splitByChar, splitLines, toInt)

mapNumbers :: String -> [[Int]]
mapNumbers = map (map toInt . splitByChar '\t') . splitLines

maxMinDiff :: [Int] -> Int
maxMinDiff xs = maximum xs - minimum xs

fold :: [[Int]] -> Int
fold = sum . map maxMinDiff

solve :: String -> Int
solve = fold . mapNumbers

main :: IO ()
main = print . solve =<< readFile "2017-02.txt"
