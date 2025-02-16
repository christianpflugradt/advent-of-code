main :: IO ()
main =
    let isPrime n = n > 1 && all (\i -> n `mod` i /= 0) [2..(floor . sqrt . fromIntegral $ n)]
        b = 84 * 100 - (-100000)
        c = b - (-17000)
        step = 17
    in print . length $ filter (not . isPrime) [b,(b+step)..c]