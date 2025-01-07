import qualified Data.Vector as V
import AocCommon (inc, splitLines, toInt)

data InstructionContext = InstructionContext {
    pos :: Int,
    steps :: Int,
    instructions :: V.Vector Int
} deriving (Show)

process :: InstructionContext -> InstructionContext
process context@InstructionContext { pos = pos, steps = steps, instructions = ins }
    | outOfBounds = context
    | otherwise = process InstructionContext {
            pos = newPos,
            steps = inc steps,
            instructions = ins V.// [(pos, inc jumps)]
        }
    where
        jumps = ins V.! pos
        newPos = pos + jumps
        outOfBounds = pos < 0 || pos >= length ins

solve :: String -> Int
solve content = steps $ process InstructionContext {
    pos = 0,
    steps = 0,
    instructions = V.fromList $ map toInt $ splitLines content
}

main :: IO ()
main = print . solve =<< readFile "2017-05.txt"
