import Data.List (any)
import Data.Maybe(fromJust)
import Data.Tuple(swap)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

data Instruction arg  = ACC arg
                      | JMP arg
                      | NOP arg
  deriving Show

parseInstruction :: String -> Instruction Int
parseInstruction str =
    instr argAsInt
  where
    table = [("acc", ACC)
            ,("jmp", JMP)
            ,("nop", NOP)]
    [name, arg] = words str
    instr       = fromJust $ lookup name table
    argAsInt    = (read $ map (\c -> if c == '+' then ' ' else c) arg) :: Int

data State = State {
  instructions :: [Instruction Int],
  currentIndex :: Int,
  accumulator  :: Int,
  runCounts    :: [Int]
} deriving Show

data EndReason = Again | Outside deriving (Show, Eq)

runInstruction :: State -> Instruction Int -> State
runInstruction (State instr index acc rc) instruction =
  case instruction of
    ACC arg -> State instr (index + 1)    (acc + arg) rc
    JMP arg -> State instr (index + arg)  acc         rc
    NOP arg -> State instr (index + 1)    acc         rc


setValueAt :: [t] -> Int -> (t -> t) -> [t]
setValueAt list index valueFunc = 
  (take index list)
  ++ [valueFunc (list !! index)] 
  ++ (drop (index + 1) list)

increaseRunCount :: State -> State
increaseRunCount (State instr index acc runCounts) =
    State instr index acc (setValueAt runCounts index (+1))

step :: State -> Either EndReason State
step state@(State instructions index accumulator runCounts) =
  if index >= length instructions
    then Left Outside
    else
      if (runCounts !! index) > 0
        then Left Again
        else Right
          $ runInstruction (increaseRunCount state) (instructions !! index)      

run :: State -> (State, EndReason)
run state =
    case nextState of
      Right nextState  -> run nextState
      Left  endReason  -> (state, endReason)
  where
    nextState = step state

main :: IO ()
main = do
  input <- getInput
  let instructions = map parseInstruction input
  let zeroRunCounts = replicate (length instructions) 0

  print
    $ accumulator . fst
    $ run
    $ State instructions 0 0 zeroRunCounts

  let instructionsWithIndexes = zip [0..] instructions
  let changes = [(i, NOP arg) | (i, (JMP arg)) <- instructionsWithIndexes]
             ++ [(i, JMP arg) | (i, (NOP arg)) <- instructionsWithIndexes]

  print
    $ map (accumulator . fst)
    $ filter ((== Outside) . snd)
    $ map run
    $ map (\instr -> State instr 0 0 zeroRunCounts)
    $ map (\(index, newInstr) -> setValueAt instructions index (const newInstr))
    $ changes