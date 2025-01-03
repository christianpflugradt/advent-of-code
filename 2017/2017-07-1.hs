import Data.Char (isAlpha)
import Data.List (find)
import AocCommon (splitLines)

splitWords :: String -> [String]
splitWords "" = [""]
splitWords str = foldr splitStep [""] str
    where
        splitStep char acc@(x:xs)
            | isAlpha char = (char : x) : xs
            | null x = acc
            | otherwise = [] : acc

findRoot :: [[String]] -> String
findRoot programs = case find (\x -> not (x `elem` right)) left of
        Just res -> res
        Nothing -> error "no root found"
    where
        left = map head programs
        right = concatMap tail programs

solve :: String -> String
solve = findRoot . map splitWords . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-07.txt"
