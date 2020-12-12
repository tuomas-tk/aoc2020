import Data.List (sort)
import Data.Maybe (mapMaybe, fromJust)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

data State = State {
  ship :: Loc,
  waypoint :: Loc
} deriving (Show)

data Loc = Loc {
  lat :: Int,
  lon :: Int
} deriving (Show)

rotateClockwise :: Loc -> Loc
rotateClockwise (Loc lat lon) = Loc (-lon) lat

rotateCounterClockwise :: Loc -> Loc
rotateCounterClockwise (Loc lat lon) = Loc lon (-lat)

rotateClockwiseN :: Loc -> Int -> Loc
rotateClockwiseN loc 0 = loc
rotateClockwiseN loc n = rotateClockwiseN (rotateClockwise loc) (n-1)

rotateCounterClockwiseN :: Loc -> Int -> Loc
rotateCounterClockwiseN loc 0 = loc
rotateCounterClockwiseN loc n = rotateCounterClockwiseN (rotateCounterClockwise loc) (n-1)

runInstruction :: State -> String -> State
runInstruction (State ship (Loc lat lon)) ('N' : distance) = State ship (Loc (lat + read distance) lon)
runInstruction (State ship (Loc lat lon)) ('S' : distance) = State ship (Loc (lat - read distance) lon)
runInstruction (State ship (Loc lat lon)) ('E' : distance) = State ship (Loc lat (lon + read distance))
runInstruction (State ship (Loc lat lon)) ('W' : distance) = State ship (Loc lat (lon - read distance))

runInstruction (State ship waypoint) ('R' : angle) = State ship (rotateClockwiseN        waypoint (div (read angle) 90))
runInstruction (State ship waypoint) ('L' : angle) = State ship (rotateCounterClockwiseN waypoint (div (read angle) 90))

runInstruction (State (Loc sLat sLon) wp@(Loc wpLat wpLon)) ('F' : distance) =
    State (Loc (sLat + (wpLat * (read distance))) (sLon + (wpLon * (read distance)))) wp

runInstruction state _ = state

manhattanDistance :: State -> Int
manhattanDistance (State (Loc lat lon) _) = (abs lat) + (abs lon)

main :: IO ()
main = do
  input <- getInput
  
  let finalState = foldl runInstruction (State (Loc 0 0) (Loc 1 10)) input

  print $ manhattanDistance finalState