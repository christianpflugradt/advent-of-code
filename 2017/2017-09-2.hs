import AocCommon (dec, inc)

data State = GROUP | GARBAGE | CANCEL deriving (Eq)

calculateScore :: String -> Int
calculateScore "" = 0
calculateScore str = parse (head str, tail str) GROUP 0
    where
        parse (_, []) _ total = total
        parse (c, n) state total
                | inCancel    = parse next GARBAGE total
                | startCancel = parse next CANCEL  total
                | endGarbage  = parse next GROUP   total
                | inGarbage   = parse next GARBAGE (inc total)
                | c == '<'    = parse next GARBAGE total
                | c == '{'    = parse next GROUP   total
                | c == '}'    = parse next GROUP   total
                | otherwise   = parse next GROUP   total
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
