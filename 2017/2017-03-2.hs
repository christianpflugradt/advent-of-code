import qualified Data.Map as Map

type Grid = Map.Map (Int, Int) Int

data Direction = R | U | L | D deriving (Show, Enum, Eq)

data FieldContext = FieldContext {
    value :: Int,
    pos :: (Int, Int), -- (x, y)
    direction :: Direction,
    steps :: (Int, Int), -- (remaining, total)
    grid :: Grid
} deriving (Show)

findField :: Int -> FieldContext -> FieldContext
findField n context
    | nextVal < n = findField n nextFieldContext
    | otherwise = nextFieldContext
    where
        corner = (fst $ steps context) == 0
        nextVal = fieldValue nextPos (grid context)
        nextPos = if corner
                  then nextPosition (nextDirection (direction context)) (pos context)
                  else nextPosition (direction context) (pos context)
        nextDir = if corner then nextDirection (direction context) else direction context
        nextFieldContext = FieldContext {
             value = nextVal,
             pos = nextPos,
             direction = nextDir,
             steps = nextStep (direction context) (steps context),
             grid = Map.insert nextPos nextVal (grid context)
         }

nextPosition :: Direction -> (Int, Int) -> (Int, Int)
nextPosition d (x, y)
    | d == R = (x + 1, y)
    | d == U = (x, y + 1)
    | d == L = (x - 1, y)
    | d == D = (x, y - 1)

nextDirection :: Direction -> Direction
nextDirection R = U
nextDirection U = L
nextDirection L = D
nextDirection D = R

nextStep :: Direction -> (Int, Int) -> (Int, Int)
nextStep direction (remaining, total)
    | remaining > 0             = (remaining - 1, total)
    | direction `elem` [U, D]   = (total, total + 1)
    | otherwise                 = (total - 1, total)

fieldValue :: (Int, Int) -> Grid -> Int
fieldValue (x, y) grid = sum [Map.findWithDefault 0 (xn, yn) grid | xn <- [x-1..x+1], yn <- [y-1..y+1]]

solve :: Int -> Int
solve n = value (findField n FieldContext {
    value = 1,
    pos = (0, 0),
    direction = R,
    steps = (1, 1),
    grid = Map.insert (0, 0) 1 (Map.empty :: Grid)
})

main :: IO ()
main = print $ solve 325489
