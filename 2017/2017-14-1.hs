import Data.Bits (xor)
import Data.Char (intToDigit, ord)
import Data.List (intercalate)
import Numeric (showIntAtBase)
import Text.Printf (printf)
import AocCommon (count, inc, splitByChar, toInt)

hash :: ([Int], [Int], Int, Int) -> ([Int], [Int], Int, Int)
hash ([], rope, currentPos, skipSize) = ([], rope, currentPos, skipSize)
hash ((len:lens), rope, currentPos, skipSize) = knotTie (len:lens) len lens rope currentPos skipSize
    where
        knotTie original len lens rope currentPos skipSize
            | null lens = (original, knotTied, nextPos, (inc skipSize))
            | otherwise = knotTie original (head lens) (tail lens) knotTied nextPos (inc skipSize)
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

hashRepeatedly :: [Int] -> [Int]
hashRepeatedly input = rope
    where
        completeInput = input ++ [17, 31, 73, 47, 23]
        (_, rope, _, _) = (iterate hash (completeInput, [0..255], 0, 0)) !! 64

toBlocks :: [Int] -> [[Int]]
toBlocks = foldr step []
    where
        step int [] = [[int]]
        step int acc@(x:xs)
            | length x == 16 = [int] : acc
            | otherwise = (int : x) : xs

intToBits :: Int -> String
intToBits i = printf "%08s" (showIntAtBase 2 intToDigit i "")

rowHash :: String -> Int -> String
rowHash s i = intercalate "" . map (intToBits . foldl xor 0) . toBlocks . hashRepeatedly . map ord $ s ++ "-" ++ show i

solve :: String -> Int
solve str = sum $ map (count '1' . rowHash str) [0..127]

main :: IO ()
main = print . solve $ "ffayrhll"
