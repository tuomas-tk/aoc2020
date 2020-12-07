import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Debug.Trace

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

type Bag = (String, [String])

data BagWithBags = BagWithBags {
  name :: String,
  bags :: [BagWithBags]
} deriving (Show)

parseContents :: [String] -> [String]
parseContents []            = []
parseContents ("no" : rest) = []
parseContents contents      = 
    (take (read num) $ repeat (name1 ++ name2)) ++ (parseContents rest)
  where
    [num, name1, name2] = take 3 contents
    rest = drop 4 contents

parseLine :: String -> Bag
parseLine line =
    ((concat first), parseContents (drop 2 rest))
  where
    (first, rest) = break (=="bags") (words line)

getInnerBags :: [Bag] -> String -> BagWithBags
getInnerBags allBags name =
    BagWithBags
      name
      (
        map (getInnerBags allBags)
        $ fromJust
        $ lookup name allBags
      )

{-containsBag :: String -> BagWithBags -> Bool
containsBag name (BagWithBags bagName contents) =
  if (name == bagName)
    then True
    else or $ map (containsBag name) contents-}
      -- if (any (==bagName) callstack)
      --  then False
      --  else any (containsBag name (bagName : traceShowId callstack)) contents

getInnerBagNames :: BagWithBags -> Set.Set String
getInnerBagNames (BagWithBags bagName contents) =
  traceShowId $ foldl Set.union (Set.singleton bagName) (map getInnerBagNames contents)

containsBagName :: String -> Set.Set String -> Bool
containsBagName name contents = Set.member name contents

containsBag :: Int -> BagWithBags -> Bool
containsBag level (BagWithBags bagName contents)
  | bagName == "shinygold"     = traceShow level True
  | otherwise                  = or $ map (containsBag (level + 1)) contents

countBags :: BagWithBags -> Int
countBags (BagWithBags bagName contents) =
    1 + (sum $ map countBags contents)

main :: IO ()
main = do
  input <- getInput
  let bags = map parseLine input
  let bagsWithFlatContents = map (\(name, _) -> (name, getInnerBags bags name)) bags
  print $ length bagsWithFlatContents
  {-print
    -- $ filter (\(_, contents) -> traceShowId (containsBag 0 contents))
    $ filter (containsBagName "shinygold")
    $ map (getInnerBagNames . snd)
    $ take 1
    $ filter ((/= "shinygold") . fst)
    $ bagsWithFlatContents-}
  {-print $ lookup "shinygold" bags-}
  print
    $ ( countBags
      $ snd
      $ head
      $ (filter (\(name, _) -> name == "shinygold") bagsWithFlatContents)
      ) - 1
  {- print $ getInnerBags bags "shinygold" -}
  {-print $ length bagsWithFlatContents-}
