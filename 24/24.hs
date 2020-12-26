import Data.Map (Map, (!), fromList, union, findMax, size)
import Data.List (foldl')

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

parseDirections :: String -> [Hexagon]
parseDirections "" = []
parseDirections ('e'       : rest) = ( 2,  0) : parseDirections rest
parseDirections ('s' : 'e' : rest) = ( 1, -2) : parseDirections rest
parseDirections ('s' : 'w' : rest) = (-1, -2) : parseDirections rest
parseDirections ('w'       : rest) = (-2,  0) : parseDirections rest
parseDirections ('n' : 'w' : rest) = (-1,  2) : parseDirections rest
parseDirections ('n' : 'e' : rest) = ( 1,  2) : parseDirections rest


type Hexagon = (Int, Int)

getMaxBounds :: [Hexagon] -> ((Int, Int), (Int, Int))
getMaxBounds hexagons =
    ( (minimum a, maximum a)
    , (minimum b, maximum b) )
  where
    a = map (\(a,_) -> a) hexagons
    b = map (\(_,b) -> b) hexagons

runRound :: [Hexagon] -> [Hexagon]
runRound input =
    filter (getNewState input)
      $ [
        (x,y)
        | x <- [xMin - 2 .. xMax + 2]
        , y <- [yMin - 2 .. yMax + 2]
        , mod y 2 == 0
        , mod x 2 == mod (div y 2) 2
      ]
  where
    ((xMin, xMax), (yMin, yMax)) = getMaxBounds input

getNewState :: [Hexagon] -> Hexagon -> Bool
getNewState hexagons hexagon =
    if isActive
      then not (length activeNeighbors == 0 || length activeNeighbors > 2)
      else length activeNeighbors == 2
  where
    activeNeighbors = filter (isCloseTo hexagon) hexagons
    isActive  = any (==hexagon) hexagons

isCloseTo :: Hexagon -> Hexagon -> Bool
isCloseTo h1@(x1,y1) h2@(x2,y2) =
  (abs (x1 - x2)) + (abs (y1 - y2)) <= 3 &&
  h1 /= h2

runTimes :: (a -> a) -> Int -> a -> a
runTimes _ 0 val = val
runTimes f n val = runTimes f (n-1) (f val)


main :: IO ()
main = do
  inputStr <- getInput
  
  let flips = map (foldl' (\(ax, ay) (bx, by) -> (ax+bx, ay+by)) (0,0))
            $ map parseDirections
            $ inputStr

  let onlyBlack = filter (\f -> mod (length $ filter (==f) flips) 2 == 1) flips

  print $ length $ onlyBlack

  print $ length $ runTimes runRound 100 onlyBlack