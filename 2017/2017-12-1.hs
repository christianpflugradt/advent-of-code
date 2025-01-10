import qualified Data.Map as M
import qualified Data.Set as S
import AocCommon (splitWords, splitLines, toInt, trimTrailing)

toProgramMap :: [[String]] -> M.Map Int [Int]
toProgramMap = toMap M.empty
    where
        toMap m [] = m
        toMap m list = toMap (M.insert program programs m) (tail list)
            where
                current = head list
                program = toInt . head $ current
                programs = map (toInt . trimTrailing ',') (drop 2 current)

groupOf :: Int -> M.Map Int [Int] -> S.Set Int
groupOf i m = collect [i] S.empty m
    where
        collect [] set _ = set
        collect ids set m = collect updatedIds (S.insert current set) m
            where
                current = head ids
                candidates = M.findWithDefault [] current m
                updatedIds = tail ids ++ filter (not . flip S.member set) candidates

solve :: String -> Int
solve = S.size . groupOf 0 . toProgramMap . map splitWords . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-12.txt"
