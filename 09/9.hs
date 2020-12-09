import Data.List (any)
import Data.Maybe(fromJust)
import Data.Tuple(swap)
import Data.Set (Set, fromList, member)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

possibleSums :: [Int] -> Set Int
possibleSums list = fromList [ x + y | x <- list, y <- list, x /= y ]

isOk :: [Int] -> Bool
isOk list =
    member (list !! 25) sums
  where
    sums = possibleSums (take 25 list)

findRangesWithSum :: [Int] -> Int -> [[Int]]
findRangesWithSum list target =
    [
      (take (b-a+1) (drop a list))
        | a <- [    0..(length list - 1)]
        , b <- [(a+2)..(length list - 1)]
        , (cumulative !! (b+1)) - (cumulative !! a) == target
    ]
  where
    cumulative = (scanl (+) 0 list)

main :: IO ()
main = do
  input <- getInput
  let numbers = map read input :: [Int]

  let invalid = fst
              $ head
              $ filter (not . snd)
              $ map (\l -> (l !! 25, isOk l))
              $ map (\l -> drop l numbers)
              $ [0..(length numbers) - 26]

  print invalid

  let range = head $ findRangesWithSum numbers invalid
  print $ minimum range + maximum range