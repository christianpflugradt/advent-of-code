import Data.Char (isAlpha, isDigit)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import AocCommon (count, splitLines, toInt)

data Program = Program {
    name :: String,
    subPrograms :: [Program],
    weight :: Int
} deriving (Show)

splitAlphanumerics :: String -> [String]
splitAlphanumerics "" = [""]
splitAlphanumerics str = foldr splitStep [""] str
    where
        splitStep char acc@(x:xs)
            | isAlpha char || isDigit char = (char : x) : xs
            | null x = acc
            | otherwise = [] : acc

findRoot :: [[String]] -> String
findRoot programs = case find (\x -> not (x `elem` right)) left of
        Just res -> res
        Nothing -> error "no root found"
    where
        left = map head programs
        right = concatMap tail programs

totalWeight :: Program -> Int
totalWeight = \p -> weight p + sum (map totalWeight (subPrograms p))

programTree :: String -> [[String]] -> Program
programTree root programs = Program {
            name = head programData,
            weight = toInt (programData !! 1),
            subPrograms = map (\x -> programTree x programs) (drop 2 programData)
        }
    where
        programData = fromJust (find (\x -> root == head x) programs)

isBalanced :: Program -> Bool
isBalanced program = uniqueWeights < 2
    where
        uniqueWeights = S.size . S.fromList $ map totalWeight (subPrograms program)

findUnbalanced :: Program -> Maybe Int
findUnbalanced program
    | isBalanced program = Nothing
    | all isBalanced (subPrograms program) = Just $ weightCorrection + weightUnbalanced
    | otherwise = findUnbalanced . fromJust $ find (not . isBalanced) (subPrograms program)
    where
        weights = map totalWeight $ subPrograms program
        minWeight = minimum weights
        maxWeight = maximum weights
        wantedWeight = if count minWeight weights == 1 then minWeight else maxWeight
        weightCorrection = if wantedWeight == minWeight then maxWeight - minWeight else minWeight - maxWeight
        weightUnbalanced = weight . fromJust $ find (\x -> totalWeight x == wantedWeight) (subPrograms program)


solve :: String -> Int
solve content = case findUnbalanced tree of
        Just res -> res
        Nothing -> error "no unbalanced node found"
    where
        programs = map splitAlphanumerics $ splitLines content
        tree = programTree (findRoot programs) programs

main :: IO ()
main = print . solve =<< readFile "2017-07.txt"
