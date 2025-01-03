import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import AocCommon (dec, findLargestWithIndex, inc, splitByChar)

redistributionCycles :: V.Vector Int -> Int
redistributionCycles banks = processCycle banks [] 0
    where
        processCycle currentBanks visited cycles
                | isJust currentBanksLookup = cycles - fromJust currentBanksLookup
                | otherwise = processCycle redistributedBanks ((currentBanks, cycles) : visited) (inc cycles)
            where
                currentBanksLookup = lookup currentBanks visited
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
solve = redistributionCycles . V.fromList . map (read :: String -> Int) . splitByChar '\t'

main :: IO ()
main = print . solve =<< readFile "2017-06.txt"
