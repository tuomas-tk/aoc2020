import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe, fromJust)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

divideByComma :: String -> [String]
divideByComma ""  = []
divideByComma str =
    this : (divideByComma $ drop ((length this) + 1) str)
  where
    this = takeWhile (/=',') str

getDepartures :: Integer -> [Integer]
getDepartures period = [0,period..]

findDeparture :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
findDeparture (currT, currPeriod) (busDelay, busPeriod) =
  ( head
    $ filter (\t -> (mod (busPeriod - mod t busPeriod) busPeriod) == mod busDelay busPeriod)
    $ [currT, (currT + currPeriod) ..]
  , currPeriod * busPeriod )

main :: IO ()
main = do
  input <- getInput

  let earliestDepart = read $ head input :: Integer
  let strBusses = divideByComma $ last input
  
  -- A
  let (bus, wait) = head
                  $ sortBy (\(_, a) (_,b) -> compare a b)
                  $ map (\bus -> (bus, bus - mod earliestDepart bus))
                  $ map read
                  $ filter (/="x")
                  strBusses
  print $ bus * wait

  -- B
  let busses = map (\(id, str) -> (id, read str :: Integer))
             $ filter (\(_, str) -> str /= "x")
             $ zip [0..] strBusses

  print $ fst $ foldl findDeparture (0, 1) busses