import Data.List (find, intercalate, transpose)
import Data.Maybe (fromJust)
import AocCommon (dec, inc, splitByChar, splitLines)

type Grid = [String]
type Square = [String]
type Signature = String

data Rule = Rule { from :: Signature, to :: Signature }

signatures :: Square -> [Signature]
signatures sq =
    let rotations = [sq, transpose (reverse sq), reverse (transpose sq), map reverse (reverse sq)]
    in  map (intercalate "/") $ rotations ++ map reverse rotations

match :: [Rule] -> [Signature] -> Square
match rules sigs = splitByChar '/' . fromJust $ fmap to (find (\rule -> from rule `elem` sigs) rules)

breakdown :: Grid -> [Square]
breakdown = concatMap verticallySliced . horizontallySliced
    where
        horizontallySliced grid = map (\s -> take size (drop s grid)) steps
            where
                steps = takeWhile (< length grid) [0,size..]
                size = if length grid `mod` 2 == 0 then 2 else 3
        verticallySliced grid = map (\s -> map (\r -> take size (drop s r)) grid) steps
            where
                steps = takeWhile (< length (head grid)) [0,size..]
                size = length grid

assemble :: [Square] -> Grid
assemble squares =
    let rows = map (\step -> take size (drop step squares)) steps
        steps = takeWhile (< length squares) [0,size..]
        size = round . sqrt . fromIntegral $ length squares
    in  concatMap (map concat . transpose) rows

pixelsTurnedOnCount :: Grid -> Int
pixelsTurnedOnCount = sum . map (length . filter (=='#'))

toRules :: String -> [Rule]
toRules =
    let toRule [from, _, to] = Rule from to
    in  map (toRule . splitByChar ' ') . splitLines

evolve :: [Rule] -> Grid -> Grid
evolve rules grid = assemble . map (match rules . signatures) $ breakdown grid

solve :: String -> Int
solve content =
    let rules = toRules content
    in  pixelsTurnedOnCount $ iterate (evolve rules) [".#.", "..#", "###"] !! 5

main :: IO ()
main = print . solve =<< readFile "2017-21.txt"
