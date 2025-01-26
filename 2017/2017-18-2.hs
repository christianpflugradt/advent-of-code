import Data.List (find)
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import AocCommon (inc, splitByChar, splitLines, toInt)

data Command = SND | SET | ADD | MUL | MOD | RCV | JGZ deriving (Eq, Read)
data IntOrChar = AnInt Int | AChar Char deriving (Eq)
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

overwrite :: a -> a -> a
overwrite _ a = a

toIntOrChar :: String -> IntOrChar
toIntOrChar str
    | head str `elem` ['a'..'z'] = AChar (head str)
    | otherwise = AnInt $ toInt str

toInstruction :: String -> Instr
toInstruction str = Instr { cmd = cmd, x = x, y = y }
    where
        parts = splitByChar ' ' str
        cmd = read (map toUpper . head $ parts) :: Command
        x = toIntOrChar (parts !! 1)
        y = if length parts == 3 then toIntOrChar (parts !! 2) else AChar ' '

applyInstr :: Instr -> V.Vector Int -> Int -> V.Vector Int
applyInstr instr@Instr { cmd = cmd, x = x, y = y } registers rcv
    | cmd == SET = update overwrite val idx registers
    | cmd == ADD = update (+) val idx registers
    | cmd == MUL = update (*) val idx registers
    | cmd == MOD = update mod val idx registers
    | cmd == RCV = update overwrite rcv idx registers
    | otherwise = registers
    where
        val = getRawValOrRegVal y registers
        idx = case x of AChar c -> charToSlot c
                        AnInt _ -> error "unexpected integer"

processToTermination :: V.Vector Int -> V.Vector Int -> [Instr] -> Int
processToTermination registersA registersB instructions = processBoth registersA registersB 0 0 Seq.empty Seq.empty 0
    where
        processBoth regsA regsB posA posB sndA sndB count
            | stepsA == 0 && stepsB == 0 = newCount
            | otherwise = processBoth newRegsA newRegsB newPosA newPosB newRcvB newSndB newCount
            where
                (newRegsA, newPosA, newRcvA, newSndA, _, stepsA) = process regsA posA sndB sndA 0 0
                (newRegsB, newPosB, newRcvB, newSndB, newCount, stepsB) = process regsB posB newSndA newRcvA count 0
        process regs pos rcv send count steps
            | (cmd instr) == RCV && Seq.null rcv = (regs, pos, rcv, send, count, steps)
            | pos < 0 || pos >= length instructions = (regs, pos, rcv, send, count, steps)
            | otherwise = process updatedRegs newPos newRcv newSend newCount (inc steps)
            where
                instr = instructions !! pos
                updatedRegs = applyInstr instr regs (Seq.index rcv 0)
                newRcv = if cmd instr == RCV then Seq.drop 1 rcv else rcv
                newSend = if cmd instr == SND then send Seq.|> getRawValOrRegVal (x instr) regs else send
                newCount = if send /= newSend then inc count else count
                newPos = if cmd instr == JGZ && (getRawValOrRegVal (x instr) regs) > 0
                         then pos + (getRawValOrRegVal (y instr) regs)
                         else inc pos

solve :: String -> Int
solve = processToTermination regsA regsB . map toInstruction . splitLines
    where
        regsA = V.fromList $ replicate 26 0
        regsB = update overwrite 1 (charToSlot 'p') . V.fromList $ replicate 26 0

main :: IO ()
main = print . solve =<< readFile "2017-18.txt"