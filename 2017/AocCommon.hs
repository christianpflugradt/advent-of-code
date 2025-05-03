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
    count,
) where

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

count :: Eq a => a -> [a] -> Int
count  x = length . filter (== x)
