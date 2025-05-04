import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import AocCommon (dec, inc, splitByChar, splitLines, splitWords, toInt)

type State = Char
type ConditionalState = (State, Bool)

data Instruction = Instruction { write :: !Bool, forward :: !Bool, next :: !State }
data Context = Context { current :: !State, remaining :: !Int, pos :: !Int, tape :: !(S.Set Int) }
type States = M.Map ConditionalState Instruction
type Program = (Context, States)

parseInstructions :: [String] -> Program
parseInstructions input = (initialContext, states)
    where
        initialContext = Context {
            current = head . last . splitWords $ input !! 0,
            remaining = toInt $ filter isDigit $ input !! 1,
            pos = 0,
            tape = S.empty
        }
        stateBlocks = splitSections (drop 2 input)
        states = M.fromList $ concatMap parseStateBlock stateBlocks
        splitSections :: [String] -> [[String]]
        splitSections [] = []
        splitSections (l:ls)
            | "In state" `isPrefixOf` l =
                let (block, rest) = splitAt 9 (l:ls)
                in block : splitSections (dropWhile null rest)
            | otherwise = splitSections ls
        parseStateBlock :: [String] -> [((State, Bool), Instruction)]
        parseStateBlock block =
            let state = head . last . splitWords $ block !! 0
                parseInstr offset write =
                    ( (state, write),
                      Instruction {
                          write = "1." `isSuffixOf` (block !! offset),
                          forward = "right." `isSuffixOf` (block !! (offset + 1)),
                          next = head . last . splitWords $ block !! (offset + 2)
                      }
                    )
            in [ parseInstr 2 False, parseInstr 6 True ]

step :: Program -> Program
step (context, states) = (newContext, states)
    where oldPos = pos context
          oldTape = tape context
          instr = states M.! (current context, oldPos `S.member` oldTape)
          newContext = Context {
              current = next instr,
              remaining = dec $ remaining context,
              pos = (if forward instr then inc else dec) oldPos,
              tape = (if write instr then S.insert else S.delete) oldPos oldTape
          }

stepRemaining :: Program -> Program
stepRemaining program
    | remaining (fst program) == 0 = program
    | otherwise                    = stepRemaining $ step program

solve :: String -> Int
solve = length . tape . fst . stepRemaining . parseInstructions . splitLines

main :: IO ()
main = print . solve =<< readFile "2017-25.txt"
