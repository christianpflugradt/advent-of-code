import Data.List (delete, maximumBy)
import Data.Ord (comparing)
import AocCommon (inc, splitByChar, splitLines, toInt)

type Component = (Int, Int)

data Bridge = Bridge { strength :: Int, pins :: Int, len :: Int, remaining :: [Component] }

toComponents :: String -> [Component]
toComponents = map (toComponent . splitByChar '/') . splitLines
    where toComponent [a, b] = (toInt a, toInt b)

initialBridge :: [Component] -> Bridge
initialBridge comps = Bridge 0 0 0 comps

matches :: Int -> [Component] -> [Component]
matches pins comps = filter (\c -> fst c == pins || snd c == pins) comps

findAllBridges :: Bridge -> [Bridge]
findAllBridges bridge =
    if parts == []
    then [bridge]
    else concat
        [ findAllBridges b | i <- [0 .. length parts - 1],
        let c = parts !! i,
        let nextPins = if pins bridge == fst c then snd c else fst c,
        let b = Bridge {
            strength = strength bridge + uncurry (+) c,
            pins = nextPins,
            len = inc $ len bridge,
            remaining = delete c (remaining bridge)
        }
        ]
    where parts = matches (pins bridge) (remaining bridge)

solve :: String -> Int
solve = strength . maximumBy (comparing (\b -> (len b, strength b))) . findAllBridges . initialBridge . toComponents

main :: IO ()
main = print . solve =<< readFile "2017-24.txt"
