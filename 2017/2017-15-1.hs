import Data.Bits ((.&.))
import Data.Char (isDigit)
import AocCommon (inc, splitLines, toInt)

data State = State { genA :: Int, genB :: Int, matched :: Bool, matchedCount :: Int }

compareLowerBits :: Int -> Int -> Bool
compareLowerBits a b = (a .&. 0xFFFF) == (b .&. 0xFFFF)

next :: State -> State
next (State a b matched c) = State an bn (compareLowerBits an bn) (if matched then inc c else c)
    where
        truncated = ((flip mod) 2147483647)
        an = truncated $ a * 16807
        bn = truncated $ b * 48271

solve :: String -> Int
solve content = matchedCount finalState
    where
        [firstNum, secondNum] = map (\x -> toInt $ filter isDigit x) $ splitLines content
        initialState = State firstNum secondNum False 0
        finalState = foldl (\state _ -> next state) initialState [0..40000000]

main :: IO ()
main = print . solve =<< readFile "2017-15.txt"
