import AocCommon (dec, inc)

data State = GROUP | GARBAGE | CANCEL deriving (Eq)

calculateScore :: String -> Int
calculateScore "" = 0
calculateScore str = parse (head str, tail str) GROUP 0 0
    where
        parse (_, []) _ _ total = inc total
        parse (c, n) state depth total
                | inCancel    = parse next GARBAGE depth       total
                | startCancel = parse next CANCEL  depth       total
                | endGarbage  = parse next GROUP   depth       total
                | inGarbage   = parse next GARBAGE depth       total
                | c == '<'    = parse next GARBAGE depth       total
                | c == '{'    = parse next GROUP   (inc depth) total
                | c == '}'    = parse next GROUP   (dec depth) (depth + total)
                | otherwise   = parse next GROUP   depth       total
            where
                next        = (head n, tail n)
                inGarbage   = state == GARBAGE
                endGarbage  = inGarbage && c == '>'
                startCancel = inGarbage && c == '!'
                inCancel    = state == CANCEL

solve :: String -> Int
solve = calculateScore

main :: IO ()
main = print . solve =<< readFile "2017-09.txt"
