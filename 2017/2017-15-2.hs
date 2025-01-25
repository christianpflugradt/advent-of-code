import Data.Bits ((.&.))
import Data.Char (isDigit)
import AocCommon (inc, splitLines, toInt)

data State = State { genA :: Int, genB :: Int, matched :: Bool, matchedCount :: Int }

compareLowerBits :: Int -> Int -> Bool
compareLowerBits a b = (a .&. 0xFFFF) == (b .&. 0xFFFF)

timesUntilMultipleOf :: Int -> Int -> Int -> Int
timesUntilMultipleOf num times mul =
    let newNum = (num * times) `mod` 2147483647
    in  if newNum `mod` mul == 0
        then newNum
        else timesUntilMultipleOf newNum times mul

next :: State -> State
next (State a b matched c) = State an bn (compareLowerBits an bn) (if matched then inc c else c)
    where
        an = timesUntilMultipleOf a 16807 4
        bn = timesUntilMultipleOf b 48271 8

solve :: String -> Int
solve content = matchedCount finalState
    where
        [firstNum, secondNum] = map (\x -> toInt $ filter isDigit x) $ splitLines content
        initialState = State firstNum secondNum False 0
        finalState = foldl (\state _ -> next state) initialState [0..5000000]

main :: IO ()
main = print . solve =<< readFile "2017-15.txt"