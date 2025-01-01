import Data.List (sort)
import AocCommon (hasDuplicates, splitByChar, splitLines)

solve :: String -> Int
solve = length . filter not . map (hasDuplicates . map sort . splitByChar ' ') . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-04.txt"