import System.IO (readFile)

split :: Char -> String -> [String]
split _ "" = [""]
split sep str = foldr f [""] str
    where
        f c acc@(x:xs)
            | c == sep = "" : acc
            | otherwise = (c : x) : xs

mapNumbers :: String -> [[Int]]
mapNumbers = map (map read . split '\t') . split '\n'

maxMinDiff :: [Int] -> Int
maxMinDiff xs = maximum xs - minimum xs

fold :: [[Int]] -> Int
fold = sum . map maxMinDiff

solve :: String -> Int
solve = fold . mapNumbers

main :: IO ()
main = print . solve =<< readFile "2017-02.txt"
