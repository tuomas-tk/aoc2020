import qualified Data.Set as Set
import Data.List (any)
import Data.Maybe(fromJust)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input2.txt"

type Bag = (String, [InnerBag])
type InnerBag = (Int, String)

parseContents :: [String] -> [InnerBag]
parseContents []            = []
parseContents ("no" : rest) = []
parseContents contents      = 
    [((read num) :: Int, name1 ++ name2)] ++ (parseContents rest)
  where
    [num, name1, name2] = take 3 contents
    rest = drop 4 contents

parseLine :: String -> Bag
parseLine line =
    ((concat first), parseContents (drop 2 rest))
  where
    (first, rest) = break (=="bags") (words line)

getInnerBags :: [Bag] -> String -> String -> [String]
getInnerBags allBags topBag name =
    (concat $ map (getInnerBags allBags topBag) $ filter (/=topBag) $ map snd $ fromJust $ lookup name allBags) ++ [name]

containsBag :: String -> [String] -> Bool
containsBag name contents = any (==name) contents

main :: IO ()
main = do
  input <- getInput
  let bags = map parseLine input
  let bagsWithFlatContents = map (\(name, _) -> (name, getInnerBags bags name name)) bags
  {-print
    $ length
    $ filter ((/= "shinygold") . fst)
    $ filter (\(_, contents) -> containsBag "shinygold" contents)
    $ bagsWithFlatContents-}

  print
    $ fromJust
    $ lookup "shinygold" bagsWithFlatContents
