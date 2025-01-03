module AocCommon (dec, findLargestWithIndex, hasDuplicates, inc, splitByChar, splitLines) where

import qualified Data.Set as Set
import qualified Data.Vector as V

-- numbers

inc :: Int -> Int
inc = (+1)

dec :: Int -> Int
dec = subtract 1

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

-- vectors

findLargestWithIndex :: Ord a => V.Vector a -> Maybe (Int, a)
findLargestWithIndex vector
        | V.null vector = Nothing
        | otherwise = Just $ findMax (0, V.head vector) 1
    where
        findMax (maxValIdx, maxVal) idx
                | idx >= V.length vector = (maxValIdx, maxVal)
                | otherwise = findMax (newMaxValIdx, newMaxVal) (inc idx)
            where
                currentVal = vector V.! idx
                newMaxVal = max currentVal maxVal
                newMaxValIdx = if currentVal > maxVal then idx else maxValIdx