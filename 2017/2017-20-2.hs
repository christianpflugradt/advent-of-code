import qualified Data.Map as M
import AocCommon (splitByChar, splitLines, toInt)

data Coordinates = Coordinates { x :: Int, y :: Int, z :: Int } deriving (Eq, Ord)
data Particle = Particle { p :: Coordinates, v :: Coordinates, a :: Coordinates } deriving (Eq, Ord)

toParticles :: String -> [Particle]
toParticles = map (toParticle . toSegments) . splitLines
    where
        toSegments = map (splitByChar ',' . takeWhile (/= '>')) . drop 1 . splitByChar '<'
        toCoords [x, y, z] = Coordinates (toInt x) (toInt y) (toInt z)
        toParticle [p, v, a] = Particle (toCoords p) (toCoords v) (toCoords a)

update :: Particle -> Particle
update (Particle p v a) = Particle np nv a
    where
        nv = Coordinates (x a + x v) (y a + y v) (z a + z v)
        np = Coordinates (x nv + x p) (y nv + y p) (z nv + z p)

withCollisionsRemoved :: [Particle] -> [Particle]
withCollisionsRemoved particles = foldMap nonColliding (M.elems grouped)
    where
        grouped = M.fromListWith (++) [(p part, [part]) | part <- particles]
        nonColliding ps = if length ps == 1 then ps else []

solve :: String -> Int
solve content = length $ foldl (\particles _ -> withCollisionsRemoved (map update particles)) (toParticles content) [0..40] -- magic number

main :: IO ()
main = print . solve =<< readFile "2017-20.txt"
