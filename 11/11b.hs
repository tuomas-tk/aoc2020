import Data.List (sort)
import Data.Maybe (mapMaybe, fromJust)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

atIndex :: [a] -> Int -> Maybe a
atIndex list index =
  if (0 <= index && index <= (length list - 1))
    then Just $ list !! index
    else Nothing

count :: (Eq a) => [a] -> a -> Int
count list elem = length $ filter (==elem) list

getStatus :: [[Char]] -> (Int, Int) -> Maybe Char
getStatus seats (x,y) =
    maybe Nothing (\row -> atIndex row x) row
  where
    row = atIndex seats y

getStatusInDirection :: [[Char]] -> (Int, Int) -> (Int, Int) -> Maybe Char
getStatusInDirection seats (x,y) (dx,dy) =
  let status = getStatus seats (x+dx, y+dy)
  in
    if (status == Just '.')
      then getStatusInDirection seats (x+dx, y+dy) (dx,dy)
      else status

getStatusAround :: [[Char]] -> (Int, Int) -> [Char]
getStatusAround seats pos = 
  mapMaybe (\move -> getStatusInDirection seats pos move)
    $ [(dx,dy) | dx <- [-1..1], dy <- [-1..1], not (dx==0 && dy==0)]

-- (free, reserved)
getCountsAround :: [[Char]] -> (Int, Int) -> (Int, Int)
getCountsAround seats pos =
    ((count status 'L'), (count status '#'))
  where
    status = getStatusAround seats pos

newStatus :: [[Char]] -> (Int, Int) -> Char -> (Int, Int) -> Char
newStatus seats pos 'L' (free, reserved)
  | reserved == 0 = '#'
  | otherwise     = 'L' 
newStatus seats pos '#' (free, reserved)
  | reserved >= 5 = 'L'
  | otherwise     = '#'
newStatus _ _ status _ = status


runRound :: [[Char]] -> [[Char]] -- [(Int, [(Int, Char)])]
runRound seats =
    map (\(y, row) -> map (\(x, status) -> newStatus seats (x,y) status (getCountsAround seats (x,y))) row)
    $ map (\(y, row) -> (y, zip [0..] row))
    $ zip [0..] seats
  where
    counts =
      map (\pos -> (pos, fromJust $ getStatus seats pos, getCountsAround seats pos))
      $ [(x,y) | x <- [0..(length $ seats !! 0)], y <- [0..(length seats)]]
    toOccupy = filter (\(pos, status, (_, reserved)) -> status == 'L' && reserved == 0) counts
    toEmpty  = filter (\(pos, status, (_, reserved)) -> status == '#' && reserved >= 4) counts


runUntilSame :: [[Char]] -> [[Char]]
runUntilSame prev =
  let next = runRound prev
  in
    if (prev == next)
      then next
      else runUntilSame next
  

main :: IO ()
main = do
  input <- getInput

  -- putStr $ unlines $ runRound input

  let endingA = runUntilSame input

  putStr $ unlines $ endingA
  print $ count (concat endingA) '#'