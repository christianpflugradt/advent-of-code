import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Vector as V
import AocCommon (dec, inc, splitByChar, toInt)

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

redistributionCycles :: V.Vector Int -> Int
redistributionCycles banks = processCycle banks S.empty 0
    where
        processCycle currentBanks visited cycles
                | currentBanks `S.member` visited = cycles
                | otherwise = processCycle redistributedBanks (S.insert currentBanks visited) (inc cycles)
            where
                (largestIndex, largestIndexVal) = case findLargestWithIndex currentBanks of
                    Just res -> res
                    Nothing -> error "empty vector encountered"
                redistributedBanks = redistribute
                    largestIndexVal
                    (inc largestIndex)
                    (currentBanks V.// [(largestIndex, 0)])

redistribute :: Int -> Int -> V.Vector Int -> V.Vector Int
redistribute blocks pos banks
        | blocks == 0 = banks
        | otherwise = redistribute
            (dec blocks)
            posNext
            (banks V.// [(posCurrent, ((banks V.! posCurrent) + 1))])
    where
        posCurrent = pos `mod` V.length banks
        posNext = (inc pos) `mod` V.length banks

solve :: String -> Int
solve = redistributionCycles . V.fromList . map toInt . splitByChar '\t'

main :: IO ()
main = print . solve =<< readFile "2017-06.txt"
