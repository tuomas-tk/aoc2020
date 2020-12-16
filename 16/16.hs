import Data.List (sort, sortBy, elemIndices, foldl')
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Map as Map
import Data.Bits
import Data.Word (Word64)
import Numeric.Natural

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

parseInput :: [String] -> [[String]]
parseInput []  = []
parseInput list =
    this : parseInput (drop (length this + 2) list)
  where
    this = takeWhile (/="") list

type Validator = Int -> Bool

rangeToValidator :: String -> Validator
rangeToValidator str =
    (\n -> (a <= n) && (n <= b))
  where
    aStr = takeWhile (/='-') str
    bStr = drop (length aStr + 1) str
    a    = read aStr :: Int
    b    = read bStr :: Int

orV :: Validator -> Validator -> Validator
orV f1 f2 = \n -> f1 n || f2 n

type Field = (String, Validator)

parseField :: String -> Field
parseField str =
    (name, orV (rangeToValidator first) (rangeToValidator second))
  where
    name  = takeWhile (/=':') str
    rest  = (drop (length name + 2) str)
    first = takeWhile (/=' ') rest
    second = (drop (length first + 4) rest)

parseTicket :: String -> [Int]
parseTicket ""  = []
parseTicket str =   
    (read this) : parseTicket (drop (length this + 1) str)
  where
    this = takeWhile (/=',') str

isValidNumber :: [Field] -> Int -> Bool
isValidNumber fields n =
  or $ map (\v -> v n) $ map snd fields

findPossibleFields :: [Field] -> [Int] -> [Field]
findPossibleFields fields numbers =
  filter (\f -> and $ map (isValidNumber [f]) numbers) fields

removeClones :: [String] -> [String] -> [String]
removeClones _      [x]  = [x]
removeClones remove list =
  foldl (\list rem -> filter (/=rem) list) list remove

runSearch :: [[String]] -> [[String]]
runSearch list =
    map (removeClones onlyOne) list
  where
    onlyOne = map head $ filter ((==1) . length) $ list
  
runWholeSearch :: [[String]] -> [[String]]
runWholeSearch list =
  if (and $ map ((==1) . length) list)
    then list
    else runWholeSearch $ runSearch list

main :: IO ()
main = do
  input <- getInput
  let (rawFields : rawOwn : rawNearby : []) = parseInput input
  let fields = map parseField rawFields
  let nearby = map parseTicket rawNearby
  let ownTicket    = parseTicket (head rawOwn)

  -- A (29759)
  print
    $ sum
    $ filter (not . isValidNumber fields)
    $ concat nearby

  -- B (1307550234719)
  let validTickets = filter (and . (map (isValidNumber fields))) nearby
  
  let possibleNames = map (map fst)
                    $ map (findPossibleFields fields)
                    $ map (\i -> map (!! i) validTickets)
                    $ [0..(length ownTicket - 1)]

  let fieldNames = map head $ runWholeSearch possibleNames

  print
    $ product
    $ map snd
    $ filter ((=="departure") . take 9 . fst)
    $ zip fieldNames ownTicket
  