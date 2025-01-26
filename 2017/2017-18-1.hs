import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import AocCommon (inc, splitByChar, splitLines, toInt)

data Command = SND | SET | ADD | MUL | MOD | RCV | JGZ deriving (Eq, Read)
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
    | head str `elem` ['a'..'z'] = AChar (head str)
    | otherwise = AnInt $ toInt str

toInstruction :: String -> Instr
toInstruction str = Instr { cmd = cmd, x = x, y = y }
    where
        parts = splitByChar ' ' str
        cmd = read (map toUpper . head $ parts) :: Command
        x = toIntOrChar (parts !! 1)
        y = if length parts == 3 then toIntOrChar (parts !! 2) else AChar ' '

applyInstr :: Instr -> V.Vector Int -> V.Vector Int
applyInstr instr@Instr { cmd = cmd, x = x, y = y } registers
    | cmd == SET = update (\_ n -> n) val idx registers
    | cmd == ADD = update (+) val idx registers
    | cmd == MUL = update (*) val idx registers
    | cmd == MOD = update mod val idx registers
    | otherwise = registers
    where
        val = getRawValOrRegVal y registers
        idx = case x of AChar c -> charToSlot c
                        AnInt _ -> error "unexpected integer"

calcLastPlayedFrequency :: V.Vector Int -> [Instr] -> Int
calcLastPlayedFrequency registers instructions = process registers 0 0
    where
       process regs pos sound
            | (cmd instr) == RCV && getRegVal (x instr) regs /= 0 = sound
            | otherwise = process updatedRegs newPos newSound
            where
                instr = if pos >= 0 && pos < length instructions
                        then instructions !! pos
                        else error "pos out of bounds"
                updatedRegs = applyInstr instr regs
                newSound = if cmd instr == SND
                           then getRegVal (x instr) regs
                           else sound
                newPos = if cmd instr == JGZ && (getRawValOrRegVal (x instr) regs) > 0
                         then pos + (getRawValOrRegVal (y instr) regs)
                         else inc pos

solve :: String -> Int
solve = calcLastPlayedFrequency (V.fromList $ replicate 26 0) . map toInstruction . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-18.txt"