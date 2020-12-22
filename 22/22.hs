import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

splitToPlayers :: [String] -> [[Int]]
splitToPlayers []    = []
splitToPlayers input =
  let
    player = map read $ drop 1 $ takeWhile (/= "") input
    rest = drop (length player + 2) input
  in
    player : splitToPlayers rest

giveCards :: [[Int]] -> Int -> [[Int]]
giveCards stacks winnerIndex =
  let
    heads        = map head stacks
    cardsToGive  = if winnerIndex == 0 then heads else reverse heads
    restOfStacks = map (drop 1) stacks
  in
    map
      (\(i, stack) ->
        if i == winnerIndex
          then stack ++ cardsToGive
          else stack
      )
      (zip [0..] restOfStacks)

runRoundA :: [[Int]] -> [[Int]]
runRoundA stacks
  | any (==[]) stacks = stacks
  | otherwise =
    let
      heads       = map head stacks
      winnerIndex = fromJust $ elemIndex (maximum $ heads) heads
    in
      giveCards stacks winnerIndex

runRoundB :: ([[[Int]]], [[Int]]) -> ([[[Int]]], [[Int]])
runRoundB (prevRounds, stacks)
  | any (==[]) stacks =
    (prevRounds, stacks)
  | any (==stacks) prevRounds =
    (prevRounds, concat stacks : take (length stacks - 1) (repeat []))
  | all (\s -> length s > head s) stacks =
    let
      results     = snd $ runGameB ([], map (\s -> take (head s) (tail s)) stacks)
      winnerIndex = fst $ head $ filter ((/=[]) . snd) $ zip [0..] results
    in
      (stacks : prevRounds, giveCards stacks winnerIndex)
  | otherwise =
    let
      heads       = map head stacks
      winnerIndex = fromJust $ elemIndex (maximum $ heads) heads
    in
      (stacks : prevRounds, giveCards stacks winnerIndex)
      
runUntilDoesNotChange :: (Eq a, Show a) => (a -> a) -> a -> a
runUntilDoesNotChange f val =
  let
    newVal = f val
  in
    if newVal == val
      then val
      else runUntilDoesNotChange f newVal

runGameA = runUntilDoesNotChange runRoundA
runGameB = runUntilDoesNotChange runRoundB

main :: IO ()
main = do
  input <- getInput
  let stacks = splitToPlayers input

  
  let end    = runGameA stacks
  let winner = head $ filter ((/=0) . length) end
  print $ sum $ zipWith (*) (reverse winner) [1..]


  let end    = snd $ runGameB ([], stacks)
  let winner = head $ filter ((/=0) . length) end
  print $ sum $ zipWith (*) (reverse winner) [1..]