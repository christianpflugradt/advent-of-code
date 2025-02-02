import Data.Char (toUpper)
import Data.List (findIndex)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map as M
import AocCommon (dec, inc, splitLines)

data Cell = Line Axis | Corner | Letter Char deriving (Eq, Show)
data Axis = Horizontal | Vertical deriving (Eq, Show)
data Direction = Along Axis | Upward | Rightward | Downward | Leftward deriving (Eq, Show)

type Coordinate = (Int, Int)
type Diagram = M.Map Coordinate Cell

charToCell :: Char -> Maybe Cell
charToCell '-' = Just (Line Horizontal)
charToCell '|' = Just (Line Vertical)
charToCell '+' = Just Corner
charToCell c
    | c `elem` ['a'..'z'] = Just (Letter $ toUpper c)
    | c `elem` ['A'..'Z'] = Just (Letter c)
    | otherwise = Nothing

toDiagram :: String -> Diagram
toDiagram = M.unions . map processLine . zip [0..] . splitLines
    where
        processLine (y, str) = foldl (insertCell y) M.empty $ zip [0..] str
        insertCell y acc (x, c) = case charToCell c of
            Just cell -> M.insert (x, y) cell acc
            Nothing -> acc

findEntrance :: String -> Coordinate
findEntrance = (,0) . fromJust . findIndex (== '|')

travelAndCollect :: Diagram -> Coordinate -> String
travelAndCollect diagram (xpos, ypos) = step (xpos, ypos, Downward) []
    where
        step (x, y, dir) letters
            | isNothing cell = reverse letters
            | Just (Line _) <- cell = step (nxt dir) letters
            | Just (Letter c) <- cell = step (nxt dir) (c : letters)
            | Just Corner <- cell, vertical = step (nxt $ Along Horizontal) letters
            | Just Corner <- cell = step (nxt $ Along Vertical) letters
            where
                cell = M.lookup (x, y) diagram
                nxt = next diagram (x, y)
                vertical = dir == Upward || dir == Downward

next :: Diagram -> Coordinate -> Direction -> (Int, Int, Direction)
next diagram (x, y) Upward = (x, dec y, Upward)
next diagram (x, y) Rightward = (inc x, y, Rightward)
next diagram (x, y) Downward = (x, inc y, Downward)
next diagram (x, y) Leftward = (dec x, y, Leftward)
next diagram (x, y) (Along Horizontal) = case M.lookup (inc x, y) diagram of
    Nothing -> (dec x, y, Leftward)
    _       -> (inc x, y, Rightward)
next diagram (x, y) (Along Vertical) = case M.lookup (x, dec y) diagram of
    Nothing -> (x, inc y, Downward)
    _       -> (x, dec y, Upward)

solve :: String -> String
solve = travelAndCollect <$> toDiagram <*> findEntrance

main :: IO ()
main = print . solve =<< readFile "2017-19.txt"
