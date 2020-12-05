-- 04 --
import Data.List

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"


divideRoundDown :: Int -> Int -> Int
divideRoundDown a b = div a b

divideRoundUp :: Int -> Int -> Int
divideRoundUp a b = 
    q + if (r > 0) then 1 else 0
  where
    (q, r) = divMod a b

  
takeHalf :: (Int, Int) -> Char -> (Int, Int)
takeHalf (start, end) 'F' = (start, divideRoundDown (start + end) 2)
takeHalf (start, end) 'B' = (divideRoundUp (start + end) 2, end)
takeHalf range        'L' = takeHalf range 'F'
takeHalf range        'R' = takeHalf range 'B'
takeHalf range         _  = range

findPos :: (Int, Int) -> String -> Int
findPos range str = fst $ foldl (\range c -> takeHalf range c) range str

parseString :: String -> (Int, Int)
parseString str = ( findPos (0, 127) (take 7 str)
                  , findPos (0,   7) (drop 7 str)
                  )

getSeatId :: (Int, Int) -> Int
getSeatId (row, col) = row * 8 + col


withDiffToNext :: [Int] -> [(Int, Int)]
withDiffToNext list = map (\(id, other) -> (id, id - other)) (zip list (0 : list))

main :: IO ()
main = do
  input <- getInput
  let seatIDs = getSeatId <$> parseString <$> input
  print $ maximum $ seatIDs
  -- print $ withDiffToNext $ sort $ seatIDs
  print $ (fst $ last $ filter (\(id, diff) -> diff /= 1) $ withDiffToNext $ sort $ seatIDs) - 1
