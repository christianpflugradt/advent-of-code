data Direction = R | U | L | D deriving (Show, Enum, Eq)

data FieldContext = FieldContext {
    value :: Int,
    pos :: (Int, Int), -- (x, y)
    direction :: Direction,
    steps :: (Int, Int) -- (remaining, total)
} deriving (Show)

sqrtFloor :: Int -> Int
sqrtFloor = floor . sqrt . fromIntegral

sqrtToField :: Int -> FieldContext
sqrtToField n = FieldContext {
        value = n * n,
        pos = if even n
              then (-half + 1, half)
              else (half, half * (-1)),
        direction = if even n then L else R,
        steps = (1, n)
    }
    where half = n `div` 2

findField :: Int -> FieldContext -> FieldContext
findField n context
    | value context < n = findField n FieldContext {
          value = value context + 1,
          pos = nextPos,
          direction = nextDir,
          steps = nextStep (direction context) (steps context)
      }
    | otherwise = context
    where
        corner = (fst $ steps context) == 0
        nextPos = if corner
                  then nextPosition (nextDirection (direction context)) (pos context)
                  else nextPosition (direction context) (pos context)
        nextDir = if corner then nextDirection (direction context) else direction context

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

solve :: Int -> Int
solve n = abs distanceX + abs distanceY
    where
        field = findField n (sqrtToField $ sqrtFloor n)
        (distanceX, distanceY) = pos field

main :: IO ()
main = print $ solve 325489
