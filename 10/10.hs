import Data.List (sort)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

countWaysSlow :: [Int] -> Int
countWaysSlow []      = 0
countWaysSlow [3]     = 1
countWaysSlow (3 : l) = countWaysSlow l
countWaysSlow list    =
    sum
     $ map (\skip -> countWaysSlow $ drop skip list)
     $ [1..(min (length ones) 3)]
  where
    ones = takeWhile (==1) list

countWays :: [Int] -> Int
countWays []      = 0
countWays [3]     = 1
countWays (3 : l) = countWays l
countWays list    =
    (countWaysSlow $ ones ++ [3]) * countWays (drop (length ones) list)
  where
    ones = takeWhile (==1) list

main :: IO ()
main = do
  input <- getInput
  let numbers = sort $ map read input :: [Int]

  let withPrevious = zip (numbers ++ [(last numbers + 3)]) (0 : numbers)

  let diffs = map (\(num, prev) -> num - prev) withPrevious

  let onejolt = length $ filter (==1) diffs
  let threejolt = length $ filter (==3) diffs

  print (onejolt * threejolt)

  print $ countWays diffs