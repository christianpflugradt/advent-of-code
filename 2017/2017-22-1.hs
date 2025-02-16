import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import AocCommon (dec, inc, splitLines)

type Pos = (Int, Int)

data Direction = U | R | D | L deriving (Eq)
data Context = Context {
    pos :: Pos,
    direction :: Direction,
    infections :: Int,
    grid :: S.Set Pos
}

directions :: [Direction]
directions = [U, R, D, L]

derivedCoordinates :: [String] -> S.Set Pos
derivedCoordinates content =
    let vsize = dec (length content)
        rowCoordinates y row = foldl (singleCoordinate y) S.empty $ zip [0..] row
        singleCoordinate y acc (x, c) = if c == '#' then S.insert (x, y) acc else acc
    in  S.unions $ zipWith rowCoordinates [vsize,(dec vsize)..] content

initialContext :: String -> Context
initialContext content =
    let rows = splitLines content
        center = (`div` 2) . length
    in  Context (center (head rows), (center rows) ) U 0 (derivedCoordinates rows)

step :: Pos -> Direction -> Pos
step p@(x, y) U = (x, inc y)
step p@(x, y) R = (inc x, y)
step p@(x, y) D = (x, dec y)
step p@(x, y) L = (dec x, y)

neighbour :: (Int -> Int) -> Direction -> Direction
neighbour f d = directions !! (f (fromJust (elemIndex d directions)) `mod` 4)

left :: Direction -> Direction
left = neighbour dec

right :: Direction -> Direction
right = neighbour inc

burst :: Context -> Context
burst c@Context { pos = pos, direction = direction, infections = infections, grid = grid } =
    let infected = pos `S.member` grid
        newDirection = (if infected then right else left) direction
        newInfections = infections + if infected then 0 else 1
        newGrid = (if infected then S.delete else S.insert) pos grid
    in  Context (step pos newDirection) newDirection newInfections newGrid

solve :: String -> Int
solve content = infections $ foldl (const . burst) (initialContext content) [1..10000]

main :: IO ()
main = print . solve =<< readFile "2017-22.txt"