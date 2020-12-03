-- 03 --

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

type Map = [[Bool]]

data State = State {
  x :: Int,
  y :: Int,
  vx :: Int,
  vy :: Int,
  trees :: Int
} deriving (Show)

parseMap :: [String] -> Map
parseMap rows = map (map (== '#')) rows


stepForward :: Map -> State -> State
stepForward m (State x y vx vy trees) = State
    (mod (x + vx) (length (m !! 0)))
    (y + vy)
    vx
    vy
    (trees + if (m !! y !! x) then 1 else 0)

run :: Map -> State -> State
run m state@(State _ y _ _ _)
  | y >= (length m) = state
  | otherwise       = run m (stepForward m state)

main :: IO ()
main = do
  input <- getInput
  let theMap = parseMap input
  let initialStates = [ State 0 0 1 1 0
                      , State 0 0 3 1 0
                      , State 0 0 5 1 0
                      , State 0 0 7 1 0
                      , State 0 0 1 2 0 ]
  let finalTreeCounts = map (trees . (run theMap)) initialStates
  print ("Trees with slope 3:1                 =  " ++ (show (finalTreeCounts !! 1)))
  print ("All tree counts multiplied together  =  " ++ (show (product finalTreeCounts)))
