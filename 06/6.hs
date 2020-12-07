-- 04 --
import qualified Data.Set as Set

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

splitToGroups :: [String] -> [[String]]
splitToGroups [] = []
splitToGroups list =
    first : splitToGroups(drop 1 rest)
  where
    (first, rest) = break (=="") list

countEveryoneYes :: [String] -> Int
countEveryoneYes people = length $ foldl1 Set.intersection $ map Set.fromList people
    
countAnyoneYes :: [String] -> Int
countAnyoneYes people = length $ Set.fromList $ concat people

main :: IO ()
main = do
  input <- getInput
  let groups = splitToGroups input
  print $ sum $ map countAnyoneYes   $ groups
  print $ sum $ map countEveryoneYes $ groups
