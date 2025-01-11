import Data.Ord (comparing)
import AocCommon (dec, inc, splitLines, splitWords, trimTrailing, toInt)

data Direction = UP | DOWN deriving (Eq)

data Firewall = Firewall {
    depth :: Int,
    range_ :: Int,
    pos :: Int,
    direction :: Direction
}

step :: Firewall -> Firewall
step fw
    | dir == UP && p == 0      = fw { pos = inc p, direction = DOWN }
    | dir == UP                = fw { pos = dec p }
    | dir == DOWN && p == maxP = fw { pos = dec p, direction = UP }
    | otherwise                = fw { pos = inc p }
    where
        p = pos fw
        maxP = (range_ fw) - 1
        dir = direction fw

toFirewalls :: [[String]] -> [Firewall]
toFirewalls = map toFirewall
  where
    toFirewall entry = Firewall {
        depth = toInt . trimTrailing ':' $ head entry,
        range_ = toInt $ entry !! 1,
        pos = 0,
        direction = DOWN
    }

stepToDepth :: Firewall -> Firewall
stepToDepth fw = (iterate step fw) !! (depth fw)

severityIfCaught :: Firewall -> Int
severityIfCaught fw
    | isCaught  = depth fw * range_ fw
    | otherwise = 0
    where
        isCaught = pos fw == 0

solve :: String -> Int
solve = sum . map (severityIfCaught . stepToDepth) . toFirewalls . map splitWords . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-13.txt"
