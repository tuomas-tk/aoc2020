import Data.List (elemIndices)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

calculateStep :: ([String], Int) -> Int
calculateStep ([]              , prev) = prev
calculateStep (("+" : n : rest), prev) = calculateStep (rest, prev + read n)
calculateStep (("*" : n : rest), prev) = calculateStep (rest, prev * read n)
calculateStep ((str : rest)    , prev) = calculateStep (rest, read str)

calculate :: String -> Int
calculate str = calculateStep (words str, 0)

parseParentheses :: String -> String
parseParentheses str =
    if length opening == 0
      then show $ calculate str
      else parseParentheses
           $ take (matchingOpening) str
           ++ show (calculate between)
           ++ drop (firstClosing + 1) str
  where
    opening = elemIndices '(' str
    closing = elemIndices ')' str
    firstClosing = head closing
    matchingOpening = last $ filter (<firstClosing) opening
    between = take (firstClosing - matchingOpening - 1) $ drop (matchingOpening + 1) str

main :: IO ()
main = do
  input <- getInput
  
  let values = map (read . parseParentheses) input :: [Int]

  print $ sum values