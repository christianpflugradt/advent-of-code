module AocCommon (
    -- numbers
    inc,
    dec,
    -- strings
    trimTrailing,
    -- split
    splitByChar,
    splitLines,
    splitWords,
    -- conversions
    toInt,
    -- lists
    hasDuplicates,
    count,
    -- vectors
    findLargestWithIndex
) where

import qualified Data.Set as Set
import qualified Data.Vector as V

-- numbers

inc :: Int -> Int
inc = (+1)

dec :: Int -> Int
dec = subtract 1

-- string

trimTrailing :: Char -> String -> String
trimTrailing c s = if not (null s) && last s == c then init s else s

-- split

splitLines :: String -> [String]
splitLines str = splitByChar '\n' str

splitByChar :: Char -> String -> [String]
splitByChar _ "" = [""]
splitByChar sep str = foldr splitStep [""] str
    where
        splitStep char acc@(x:xs)
            | char == sep = "" : acc
            | otherwise = (char : x) : xs

splitWords :: String -> [String]
splitWords = splitByChar ' '

-- conversions

toInt :: String -> Int
toInt = read

-- lists

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = checkForDuplicates Set.empty
    where
        checkForDuplicates _ [] = False
        checkForDuplicates set (x:xs)
            | x `Set.member` set = True
            | otherwise = checkForDuplicates (Set.insert x set) xs

count :: Eq a => a -> [a] -> Int
count  x = length . filter (== x)

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