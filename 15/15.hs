import Data.List (sort, sortBy, elemIndices, foldl')
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Map as Map
import Data.Bits
import Data.Word (Word64)
import Numeric.Natural

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

parseRow :: String -> [Int]
parseRow ""  = []
parseRow str =
    (read this) : parseRow (drop (length this + 1) str)
  where
    this = takeWhile (/=',') str

-- A

getNext :: [Int] -> Int
getNext list =
  let
    prev = last list
    said = elemIndices prev list
  in
    if (length said == 1)
      then 0
      else (last said) - (said !! (length said - 2))

getNextList :: [Int] -> [Int]
getNextList list = list ++ [getNext list]

-- B

type Memory = Map.Map Int Int
type State = (Memory, Int, Int)

getNextFast :: State -> State
getNextFast (mem, prevI, prevNum) =
    ( Map.insert prevNum prevI mem
    , prevI + 1
    , distToPrev )
  where
    distToPrev  = case (Map.lookup prevNum mem) of
                    Just oldIndex -> prevI - oldIndex
                    Nothing       -> 0

trd :: (a,b,c) -> c
trd (_,_,c) = c

runTimes :: (a -> a) -> Int -> a -> a
runTimes _ 0 val = val
runTimes f n val = runTimes f (n-1) (f val)

main :: IO ()
main = do
  input <- getInput
  let numbers = parseRow $ head input

  -- A
  print $ last $ runTimes getNextList (2020 - length numbers) numbers

  -- B (needs to be compiled with ghc to run without stack overflow error)
  let initState = ( Map.fromList $ zip numbers [1..]
                  , length numbers
                  , last numbers )
  
  print $ trd $ runTimes getNextFast (30000000 - length numbers) initState

  