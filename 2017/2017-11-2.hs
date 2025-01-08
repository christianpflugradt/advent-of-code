import AocCommon (splitByChar)

type Position = (Int, Int, Int, Int)

toPosition :: String -> (Position)
toPosition "n"  = (1, 0, -1, 0)
toPosition "ne" = (0, 1, -1, 0)
toPosition "se" = (-1, 1, 0, 0)
toPosition "s"  = (-1, 0, 1, 0)
toPosition "sw" = (0, -1, 1, 0)
toPosition "nw" = (1, -1, 0, 0)
toPosition _    = error "unsupported position"

add :: Position -> Position -> Position
add (x1, y1, z1, d) (x2, y2, z2, _) = (nx, ny, nz, max d (distance newPos))
    where
        newPos@(nx, ny, nz, _) = (x1 + x2, y1 + y2, z1 + z2, 0)

distance :: Position -> Int
distance (x, y, z, _) = (abs x + abs y + abs z) `div` 2

solve :: String -> Int
solve content = distance
    where
        (_, _, _, distance) = foldl add (0, 0, 0, 0) . map toPosition $ splitByChar ',' content

main :: IO ()
main = print . solve =<< readFile "2017-11.txt"
