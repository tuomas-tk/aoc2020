import Data.Map (Map, (!), fromList, union, findMax, size)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

insertToDestination :: Map Int Int -> Int -> Int -> [Int] -> Map Int Int
insertToDestination cups curr dest picked
  | any (==dest) picked = insertToDestination cups curr (dest - 1) picked
  | dest < 1            = insertToDestination cups curr (fst $ findMax cups) picked
  | otherwise           =
    union
      (fromList [(curr,        cups ! last picked)
                ,(dest,        head picked       )
                ,(last picked, cups ! dest       )])
      cups

runRound :: (Map Int Int, Int) -> (Map Int Int, Int)
runRound (cupMap, current) =
  let
    picked      = take 3 $ takeFromMap cupMap current
    newCupMap   = insertToDestination cupMap current (current - 1) picked
    nextCurrent = newCupMap ! current
  in
    (newCupMap, nextCurrent)

takeFromMap :: Map Int Int -> Int -> [Int]
takeFromMap m prev =
  let next = m ! prev
  in  next : takeFromMap m next

mapToList :: (Map Int Int) -> [Int]
mapToList map = take (size map) $ takeFromMap map 1

runTimes :: (a -> a) -> Int -> a -> a
runTimes _ 0 val = val
runTimes f n val = runTimes f (n-1) (f val)

main :: IO ()
main = do
  let inputStr = "215694783"
  --let inputStr = "389125467"

  let input = map (read . (:[])) inputStr :: [Int]
  let inputMap = fromList $ zip (last input : input) (input)

  let (result, _) = runTimes runRound 100 (inputMap, head input)
  putStrLn $ concat $ map show $ mapToList result


  let inputB = input ++ [(maximum input + 1)..1000000]
  let inputBMap = fromList $ zip (last inputB : inputB) (inputB)

  let (resultB, _) = runTimes runRound 10000000 (inputBMap, head inputB)
  print $ product $ take 2 $ takeFromMap resultB 1