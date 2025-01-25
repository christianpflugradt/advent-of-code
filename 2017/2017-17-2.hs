import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)
import AocCommon (inc)

createBufferAndGetPosAfterZero :: Int -> Int
createBufferAndGetPosAfterZero stepSize = process (Seq.fromList [0]) 0 [1..50000000]
    where
        process buffer pos [] =
            let posAfterZero = (flip mod) (Seq.length buffer) . inc . fromJust . (flip Seq.elemIndexL) buffer $ 0
            in fromJust $ Seq.lookup posAfterZero buffer
        process buffer pos insertions =
            let newPos = (pos + stepSize + 1) `mod` Seq.length buffer
                (before, after) = Seq.splitAt newPos buffer
                newBuffer = (before Seq.|> head insertions) Seq.>< after
            in  process newBuffer newPos (tail insertions)

solve :: Int -> Int
solve = createBufferAndGetPosAfterZero

main :: IO ()
main = print . solve $ 386
