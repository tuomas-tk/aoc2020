import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Map as Map
import Data.Bits
import Data.Word (Word64)
import Numeric.Natural

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

data Instruction = Mask String
                 | Memory Integer Integer
                 deriving (Show)

type Mask = String

parseRow :: String -> Instruction
parseRow str =
  if (take 4 str == "mask")
    then Mask (drop 7 str)
    else Memory
      (read (takeWhile (/=']') (drop 4 str)))
      (read (drop ((length (takeWhile (/='=') str)) + 1) str))

applyMask :: String -> Integer -> Integer
applyMask mask value =
  let
    zipped = zip [35,34..0] mask
    oneMask = foldl (.|.) 0 $ map (\(i,c) -> if c == '1' then bit i else 0) zipped :: Word64
    zeroMask = foldl (.|.) 0 $ map (\(i,c) -> if c == '0' then bit i else 0) zipped :: Word64
  in
    fromIntegral (((fromIntegral value :: Word64) .|. oneMask) .&. (complement zeroMask))

type State = (String, Map.Map Integer Integer)

runStep :: Instruction -> State -> State
runStep (Mask mask) state@(_, memory) =
  (mask, memory)
runStep (Memory target value) state@(mask, memory) =
  (mask, Map.insert target (applyMask mask value) memory)

main :: IO ()
main = do
  input <- getInput
  let instructions = map parseRow input

  let finalMemory = snd $ foldl (flip runStep) ("", Map.empty) instructions 
  print $ sum $ Map.elems finalMemory