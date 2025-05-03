import Data.List (sort)
import AocCommon (splitByChar, splitLines)

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = checkForDuplicates Set.empty
    where
        checkForDuplicates _ [] = False
        checkForDuplicates set (x:xs)
            | x `Set.member` set = True
            | otherwise = checkForDuplicates (Set.insert x set) xs

solve :: String -> Int
solve = length . filter not . map (hasDuplicates . map sort . splitByChar ' ') . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-04.txt"