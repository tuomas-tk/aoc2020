import Data.List (any)
import Data.Maybe(fromJust)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

type Bag = (String, [InnerBag])
type InnerBag = (Int, String)

parseBagContents :: [String] -> [InnerBag]
parseBagContents []         = []
parseBagContents ("no" : _) = []
parseBagContents contents   = 
    (read count, concat names) : (parseBagContents rest)
  where
    (count : names) = take 3 contents
    rest = drop 4 contents

parseLine :: String -> Bag
parseLine line =
    ((concat first), parseBagContents (drop 2 rest))
  where
    (first, rest) = break (=="bags") (words line)

getInnerBags :: [Bag] -> String -> [String]
getInnerBags allBags name =
    name :
    (
      concat
      $ map (getInnerBags allBags)
      $ map snd
      $ fromJust
      $ lookup name allBags
    )

countInnerBags :: [Bag] -> String -> Int
countInnerBags allBags name =
    sum
    $ map (\(count, name) -> count * (countInnerBags allBags name + 1))
    $ fromJust
    $ lookup name allBags

main :: IO ()
main = do
  input <- getInput
  let bags = map parseLine input
  
  print
    $ length
    $ filter (any (=="shinygold"))
    $ map (getInnerBags bags)
    $ filter (/= "shinygold")
    $ map fst
    $ bags

  print $ countInnerBags bags "shinygold"
