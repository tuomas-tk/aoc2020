import Data.List (sort, sortBy, elemIndices, foldl')
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Map as Map
import Data.Bits
import Data.Word (Word64)
import Numeric.Natural

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

type Cube = (Int, Int, Int)

parseInput :: [String] -> [Cube]
parseInput input =
  let
    array     = map (map (=='#')) input
    withIndex = zip [0..] (map (zip [0..]) array) 
  in
    concat $ map (\(y, row) -> map (\(x, _) -> (x,y,0)) $ filter (id . snd) row) withIndex


getMaxBounds :: [Cube] -> ((Int, Int), (Int, Int), (Int, Int))
getMaxBounds cubes =
    (
      (minimum a, maximum a),
      (minimum b, maximum b),
      (minimum c, maximum c)
    )
  where
    a = map (\(a,_,_) -> a) cubes
    b = map (\(_,b,_) -> b) cubes
    c = map (\(_,_,c) -> c) cubes

runRound :: [Cube] -> [Cube]
runRound input =
    filter (getNewState input)
      $ [
        (a,b,c)
        | a <- [aMin - 1 .. aMax + 1]
        , b <- [bMin - 1 .. bMax + 1]
        , c <- [cMin - 1 .. cMax + 1]
      ]
  where
    ((aMin, aMax), (bMin, bMax), (cMin, cMax)) = getMaxBounds input

getNewState :: [Cube] -> Cube -> Bool
getNewState cubes cube =
    if isActive
      then length activeNeighbors == 2 || length activeNeighbors == 3
      else length activeNeighbors == 3
  where
    activeNeighbors = filter (isCloseTo cube) cubes
    isActive  = any (==cube) cubes

isCloseTo :: Cube -> Cube -> Bool
isCloseTo cube1@(a1,b1,c1) cube2@(a2,b2,c2) =
  abs (a1 - a2) <= 1 &&
  abs (b1 - b2) <= 1 &&
  abs (c1 - c2) <= 1 &&
  cube1 /= cube2

runTimes :: (a -> a) -> Int -> a -> a
runTimes _ 0 val = val
runTimes f n val = runTimes f (n-1) (f val)

main :: IO ()
main = do
  input <- getInput
  let initialState = parseInput input

  print initialState

  print $ length $ runTimes runRound 6 initialState
  