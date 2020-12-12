import Data.List (sort)
import Data.Maybe (mapMaybe, fromJust)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

data Direction = North | East | South | West deriving (Enum, Show)

data State = State {
  lat :: Int,
  lon :: Int,
  dir :: Direction
} deriving (Show)

succTimes :: Direction -> Int -> Direction
succTimes dir  0 = dir
succTimes West n = succTimes (North) (n-1)
succTimes dir  n = succTimes (succ dir) (n-1)

predTimes :: Direction -> Int -> Direction
predTimes dir   0 = dir
predTimes North n = predTimes (West) (n-1)
predTimes dir   n = predTimes (pred dir) (n-1)

getVector :: Direction -> (Int, Int)
getVector North = ( 1,  0)
getVector East  = ( 0,  1)
getVector South = (-1,  0)
getVector West  = ( 0, -1)

runInstruction :: State -> String -> State
runInstruction (State lat lon dir) ('N' : distance) = State (lat + read distance) lon dir
runInstruction (State lat lon dir) ('S' : distance) = State (lat - read distance) lon dir
runInstruction (State lat lon dir) ('E' : distance) = State lat (lon + read distance) dir
runInstruction (State lat lon dir) ('W' : distance) = State lat (lon - read distance) dir
runInstruction (State lat lon dir) ('R' : angle)    = State lat lon (succTimes dir (div (read angle) 90))
runInstruction (State lat lon dir) ('L' : angle)    = State lat lon (predTimes dir (div (read angle) 90))
runInstruction (State lat lon dir) ('F' : distance) =
    State (lat + (dLat * read distance)) (lon + (dLon * read distance)) dir
  where
    (dLat, dLon) = getVector dir
runInstruction state _ = state

manhattanDistance :: State -> Int
manhattanDistance (State lat lon _) = (abs lat) + (abs lon)

main :: IO ()
main = do
  input <- getInput
  
  let finalState = foldl runInstruction (State 0 0 East) input

  print $ manhattanDistance finalState