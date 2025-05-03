import qualified Data.Set as S
import AocCommon (splitByChar, splitLines)

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = checkForDuplicates S.empty
    where
        checkForDuplicates _ [] = False
        checkForDuplicates set (x:xs)
            | x `S.member` set = True
            | otherwise = checkForDuplicates (S.insert x set) xs

solve :: String -> Int
solve = length . filter not . map (hasDuplicates . splitByChar ' ') . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-04.txt"
