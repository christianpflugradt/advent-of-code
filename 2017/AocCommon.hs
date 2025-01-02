module AocCommon (hasDuplicates, inc, splitByChar, splitLines) where

import qualified Data.Set as Set

-- numbers

inc :: Int -> Int
inc = (+1)

-- split file content

splitLines :: String -> [String]
splitLines str = splitByChar '\n' str

splitByChar :: Char -> String -> [String]
splitByChar _ "" = [""]
splitByChar sep str = foldr splitStep [""] str
    where
        splitStep char acc@(x:xs)
            | char == sep = "" : acc
            | otherwise = (char : x) : xs

-- lists

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = checkForDuplicates Set.empty
    where
        checkForDuplicates _ [] = False
        checkForDuplicates set (x:xs)
            | x `Set.member` set = True
            | otherwise = checkForDuplicates (Set.insert x set) xs
