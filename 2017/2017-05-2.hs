import qualified Data.Vector as V
import AocCommon (inc, splitLines, toInt)

calcStepsToOutOfBounds :: Int -> Int -> V.Vector Int -> Int
calcStepsToOutOfBounds pos steps instructions
    | pos < 0 || pos >= V.length instructions = steps
    | otherwise =
        let jumps = instructions V.! pos
            newPos = pos + jumps
            newInstructions = instructions V.// [(pos, updateOffset jumps)]
        in calcStepsToOutOfBounds newPos (inc steps) newInstructions

updateOffset :: Int -> Int
updateOffset offset
    | offset >= 3 = offset - 1
    | otherwise = inc offset

solve :: String -> Int
solve content = calcStepsToOutOfBounds 0 0 (V.fromList $ map toInt $ splitLines content)

main :: IO ()
main = print . solve =<< readFile "2017-05.txt"
