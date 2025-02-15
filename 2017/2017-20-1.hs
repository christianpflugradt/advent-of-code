import AocCommon (splitByChar, splitLines, toInt)

data Coordinates = Coordinates { x :: Int, y :: Int, z :: Int }
data Particle = Particle { n :: Int, p :: Coordinates, v :: Coordinates, a :: Coordinates }

toParticles :: String -> [Particle]
toParticles = zipWith toParticle [0..] . map toSegments . splitLines
    where
        toSegments = map (splitByChar ',' . takeWhile (/= '>')) . drop 1 . splitByChar '<'
        toCoords [x, y, z] = Coordinates (toInt x) (toInt y) (toInt z)
        toParticle n [p, v, a] = Particle n (toCoords p) (toCoords v) (toCoords a)

manhatten :: (Particle -> Coordinates) -> Particle -> Int
manhatten f p = abs (x c) + abs (y c) + abs (z c)
    where
        c = f p

pos, vel, acc :: Particle -> Int
pos = manhatten p
vel = manhatten v
acc = manhatten a

findClosest :: [Particle] -> Int
findClosest = n . head . by pos . by vel . by acc
    where
        by f p = let m = minimum (map f p) in filter (\n -> f n == m) p

solve :: String -> Int
solve = findClosest . toParticles

main :: IO ()
main = print . solve =<< readFile "2017-20.txt"
