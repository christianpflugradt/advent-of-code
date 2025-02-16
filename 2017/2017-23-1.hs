import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import AocCommon (inc, splitByChar, splitLines, toInt)

data Command = SET | SUB | MUL | JNZ deriving (Eq, Read)
data IntOrChar = AnInt Int | AChar Char
data Instr = Instr { cmd :: Command, x :: IntOrChar, y :: IntOrChar }

charToSlot :: Char -> Int
charToSlot char = fromEnum char - fromEnum 'a'

getRegVal :: IntOrChar -> V.Vector Int -> Int
getRegVal (AnInt i) v = v V.! i
getRegVal (AChar c) v = v V.! charToSlot c

getRawValOrRegVal :: IntOrChar -> V.Vector Int -> Int
getRawValOrRegVal (AnInt i) v = i
getRawValOrRegVal (AChar c) v = v V.! charToSlot c

update :: (Int -> Int -> Int) -> Int -> Int -> V.Vector Int -> V.Vector Int
update op val idx v = v V.// [(idx, getRegVal (AnInt idx) v `op` val)]

toIntOrChar :: String -> IntOrChar
toIntOrChar str
    | head str `elem` ['a'..'h'] = AChar (head str)
    | otherwise = AnInt $ toInt str

toInstruction :: String -> Instr
toInstruction str = Instr { cmd = cmd, x = x, y = y }
    where
        parts = splitByChar ' ' str
        cmd = read (map toUpper . head $ parts) :: Command
        x = toIntOrChar (parts !! 1)
        y = toIntOrChar (parts !! 2)

applyInstr :: Instr -> V.Vector Int -> V.Vector Int
applyInstr instr@Instr { cmd = cmd, x = x, y = y } registers
    | cmd == SET = update (\_ n -> n) val idx registers
    | cmd == SUB = update (-) val idx registers
    | cmd == MUL = update (*) val idx registers
    | otherwise = registers
    where
        val = getRawValOrRegVal y registers
        idx = case x of AChar c -> charToSlot c
                        AnInt _ -> error "unexpected integer"

mulCount :: V.Vector Int -> [Instr] -> Int
mulCount registers instructions = process registers 0 0
    where
       process regs pos muls
            | pos < 0 || pos >= length instructions = muls
            | otherwise = process updatedRegs newPos newMuls
            where
                instr = instructions !! pos
                updatedRegs = applyInstr instr regs
                newMuls = if cmd instr == MUL then inc muls else muls
                newPos = if cmd instr == JNZ && (getRawValOrRegVal (x instr) regs) /= 0
                         then pos + (getRawValOrRegVal (y instr) regs)
                         else inc pos

solve :: String -> Int
solve = mulCount (V.fromList $ replicate 8 0) . map toInstruction . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-23.txt"