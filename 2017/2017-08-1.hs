import qualified Data.Map as M
import AocCommon (splitLines, splitWords, toInt)

data Operator = INC | DEC deriving (Show)
data Condition = CondEQ | CondNEQ | CondLT | CondLTE | CondGT | CondGTE deriving (Show)

data Instruction = Instruction {
    opRegister :: String,
    operator :: Operator,
    opValue :: Int,
    condRegister :: String,
    condition :: Condition,
    condValue :: Int
} deriving (Show)

toOperator :: String -> Operator
toOperator "inc" = INC
toOperator "dec" = DEC
toOperator _ = error "unsupported operator"

toCondition :: String -> Condition
toCondition "==" = CondEQ
toCondition "!=" = CondNEQ
toCondition "<" = CondLT
toCondition "<=" = CondLTE
toCondition ">" = CondGT
toCondition ">=" = CondGTE
toCondition _ = error "unsupported operator"

parse :: String -> Instruction
parse line = Instruction {
        opRegister = s !! 0,
        operator = toOperator (s !! 1),
        opValue = toInt (s !! 2),
        condRegister = s !! 4,
        condition = toCondition (s !! 5),
        condValue = toInt (s !! 6)
    }
    where
        s = splitWords line

findMaxRegVal :: M.Map String Int -> [Instruction] -> Int
findMaxRegVal registers [] = maximum (M.elems registers)
findMaxRegVal registers instructions = findMaxRegVal updatedRegisters (tail instructions)
    where
        instruction = head instructions
        applicable = evaluateCondition
                     (M.findWithDefault 0 (condRegister instruction) registers)
                     (condition instruction)
                     (condValue instruction)
        updatedVal = operationApplied
                     (M.findWithDefault 0 (opRegister instruction) registers)
                     (operator instruction)
                     (opValue instruction)
        updatedRegisters = if applicable
                           then M.insert (opRegister instruction) updatedVal registers
                           else registers

evaluateCondition :: Int -> Condition -> Int -> Bool
evaluateCondition a CondEQ b = a == b
evaluateCondition a CondNEQ b = a /= b
evaluateCondition a CondLT b = a < b
evaluateCondition a CondLTE b = a <= b
evaluateCondition a CondGT b = a > b
evaluateCondition a CondGTE b = a >= b

operationApplied :: Int -> Operator -> Int -> Int
operationApplied val INC add = val + add
operationApplied val DEC add = val - add

solve :: String -> Int
solve = findMaxRegVal M.empty . map parse . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-08.txt"
