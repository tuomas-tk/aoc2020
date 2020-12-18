import Data.List (elemIndices, elemIndex)
import Data.Maybe (fromJust, isNothing)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

calculateAdditions :: [String] -> [String]
calculateAdditions list =
    if isNothing firstAddition
      then list
      else
        let
          additionIdx = fromJust firstAddition
          before      = take (additionIdx - 1) list
          after       = drop (additionIdx + 2) list
          added       = (read $ list !! (additionIdx - 1)) + (read $ list !! (additionIdx + 1))
        in
          calculateAdditions $ before ++ [show added] ++ after
  where
    firstAddition = elemIndex "+" list

calculateStep :: ([String], Int) -> Int
calculateStep ([]              , prev) = prev
calculateStep (("+" : n : rest), prev) = calculateStep (rest, prev + read n)
calculateStep (("*" : n : rest), prev) = calculateStep (rest, prev * read n)
calculateStep ((str : rest)    , prev) = calculateStep (rest, read str)

calculate :: String -> Int
calculate str = calculateStep (calculateAdditions $ words str, 0)

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