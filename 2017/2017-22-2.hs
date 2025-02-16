import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import AocCommon (dec, inc, splitLines)

type Pos = (Int, Int)

data Direction = U | R | D | L deriving (Eq)
data State = Clean | Weakend | Infected | Flagged deriving (Eq)

data Context = Context {
    pos :: Pos,
    direction :: Direction,
    infections :: Int,
    grid :: M.Map Pos State
}

directions :: [Direction]
directions = [U, R, D, L]

derivedCoordinates :: [String] -> M.Map Pos State
derivedCoordinates content =
    let vsize = dec (length content)
        rowCoordinates y row = foldl (singleCoordinate y) M.empty $ zip [0..] row
        singleCoordinate y acc (x, c) = if c == '#' then M.insert (x, y)  Infected acc else acc
    in  M.unions $ zipWith rowCoordinates [vsize,(dec vsize)..] content

initialContext :: String -> Context
initialContext content =
    let rows = splitLines content
        center = (`div` 2) . length
    in  Context (center (head rows), (center rows) ) U 0 (derivedCoordinates rows)

next :: State -> State
next Clean = Weakend
next Weakend = Infected
next Infected = Flagged
next Flagged = Clean

step :: Pos -> Direction -> Pos
step p@(x, y) U = (x, inc y)
step p@(x, y) R = (inc x, y)
step p@(x, y) D = (x, dec y)
step p@(x, y) L = (dec x, y)

neighbour :: (Int -> Int) -> Direction -> Direction
neighbour f d = directions !! (f (fromJust (elemIndex d directions)) `mod` 4)

turn :: State -> Direction -> Direction
turn Clean = neighbour dec
turn Weakend = neighbour (+0)
turn Infected = neighbour inc
turn Flagged = neighbour (+2)

burst :: Context -> Context
burst c@Context { pos = pos, direction = direction, infections = infections, grid = grid } =
    let state = M.findWithDefault Clean pos grid
        newDirection = turn state direction
        newInfections = infections + if state == Weakend then 1 else 0
        newGrid = if state == Flagged then M.delete pos grid else M.insert pos (next state) grid
    in  Context (step pos newDirection) newDirection newInfections newGrid

solve :: String -> Int
solve content = infections $ foldl (const . burst) (initialContext content) [1..10000000]

main :: IO ()
main = print . solve =<< readFile "2017-22.txt"