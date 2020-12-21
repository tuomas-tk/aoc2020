import qualified Data.Map as Map
import Data.List (intersect, nub, (\\))

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

type Food = ([String], [String])
type FoodMap = Map.Map String [String]

parseRow :: String -> Food
parseRow str =
  let
    split = words (take (length str - 1) str)
    ingredients = takeWhile (/= "(contains") split
    allergens = map (\s -> if last s == ',' then take (length s - 1) s else s) $ drop (length ingredients + 1) split
  in
    (ingredients, allergens)

possibleContainers :: Food -> FoodMap
possibleContainers (ingredients, allergens) =
  Map.fromList $ map (\a -> (a, ingredients)) allergens

removeClones :: [String] -> [String] -> [String]
removeClones _      [x]  = [x]
removeClones remove list =
  foldl (\list rem -> filter (/=rem) list) list remove

runSearch :: FoodMap -> FoodMap
runSearch foodMap =
    Map.map (removeClones onlyOne) foodMap
  where
    onlyOne = map head $ filter ((==1) . length) $ Map.elems foodMap
  
runWholeSearch :: FoodMap -> FoodMap
runWholeSearch foodMap =
  if (and $ map ((==1) . length) $ Map.elems foodMap)
    then foodMap
    else runWholeSearch $ runSearch foodMap

main :: IO ()
main = do
  input <- getInput
  let foods = map parseRow input

  let possible = foldl1 (Map.unionWith intersect) $ map possibleContainers foods
  let surelyUsed = nub $ concat $ Map.elems possible
  let allIngredients = concat $ map fst foods

  -- A
  print
    $ length
    $ filter (\i -> not $ elem i surelyUsed) allIngredients
  
  -- B
  putStrLn
    $ drop 1
    $ concat
    $ map ((","++) . head)
    $ Map.elems
    $ runWholeSearch possible