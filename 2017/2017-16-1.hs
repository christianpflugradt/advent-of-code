import Data.List (elemIndex)
import Data.Maybe (fromJust)
import AocCommon (dec, inc, splitByChar, toInt)

spin :: Int -> String -> String
spin x programs =
    let (before, after) = splitAt (length programs - x) programs
    in after ++ before

exchange :: Int -> Int -> String -> String
exchange a b programs =
    let x      = min a b
        y      = max a b
        (beforeX, fromX) = splitAt x programs
        (xVal, afterX)   = splitAt 1 fromX
        (beforeY, fromY) = splitAt (y-x-1) afterX
        (yVal, afterY)   = splitAt 1 fromY
    in beforeX ++ yVal ++ beforeY ++ xVal ++ afterY

partner :: Char -> Char -> String -> String
partner a b programs =
    let pos = fromJust . (flip elemIndex) programs
    in exchange (pos a) (pos b) programs

dance :: [String] -> String
dance moves = step "abcdefghijklmnop" moves
    where
        step programs [] = programs
        step programs (dm:rest) =
            let move = head dm
                fstArg = head . splitByChar '/' . drop 1 $ dm
                sndArg = head . drop 1 . splitByChar '/' $ dm
                updatedPrograms
                    | move == 's'  = spin (toInt . drop 1 $ dm) programs
                    | move == 'x'  = exchange (toInt fstArg) (toInt sndArg) programs
                    | move == 'p'  = partner (head fstArg) (head sndArg) programs
                    | otherwise = error "unknown move: " ++ [move]
            in step updatedPrograms rest

solve :: String -> String
solve = dance . splitByChar ','

main :: IO ()
main = print . solve =<< readFile "2017-16.txt"
