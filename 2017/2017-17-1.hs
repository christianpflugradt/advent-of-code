import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)
import AocCommon (inc)

createBufferAndGetNextPos :: Int -> Int
createBufferAndGetNextPos stepSize = process (Seq.fromList [0]) 0 [1..2017]
    where
        process buffer pos [] = fromJust $ Seq.lookup (inc pos) buffer
        process buffer pos insertions =
            let newPos = (pos + stepSize + 1) `mod` Seq.length buffer
                (before, after) = Seq.splitAt newPos buffer
                newBuffer = (before Seq.|> head insertions) Seq.>< after
            in  process newBuffer newPos (tail insertions)

solve :: Int -> Int
solve = createBufferAndGetNextPos

main :: IO ()
main = print . solve $ 386
