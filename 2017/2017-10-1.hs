import AocCommon (inc, splitByChar, toInt)

hash :: [Int] -> Int
hash [] = 0
hash (len:lens) = knotTie len lens [0..255] 0 0
    where
        knotTie len lens rope currentPos skipSize
            | null lens = (knotTied !! 0) * (knotTied !! 1)
            | otherwise = knotTie (head lens) (tail lens) knotTied nextPos (inc skipSize)
            where
                knotTied = reverseRope len rope currentPos
                nextPos = (currentPos + len + skipSize) `mod` length rope
        reverseRope len rope currentPos
            | currentPos + len <= length rope = reverseWithinBounds len rope currentPos
            | otherwise = reverseAcrossBounds len rope currentPos
            where
                reverseWithinBounds len rope pos =
                    let (before, rest) = splitAt pos rope
                        (middle, after) = splitAt len rest
                    in before ++ reverse middle ++ after
                reverseAcrossBounds len rope pos =
                    let (before, middle1) = splitAt pos rope
                        (middle2, after) = splitAt (len - length middle1) before
                        (reversed1, reversed2) = splitAt (length middle1) (reverse (middle1 ++ middle2))
                    in reversed2 ++ after ++ reversed1

solve :: String -> Int
solve = hash . map toInt . splitByChar ','

main :: IO ()
main = print . solve =<< readFile "2017-10.txt"
