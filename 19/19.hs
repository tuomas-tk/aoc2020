import qualified Data.Map as Map
import Data.Maybe (mapMaybe, isJust, fromJust, maybe)
import Data.Either (rights)

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

type RuleContent = Either [Int] String
type Rule = (Int, [RuleContent])
type RuleMap = Map.Map Int [RuleContent]

parseRule :: String -> Rule
parseRule str =
    (id, content)
  where
    id = read $ takeWhile (/=':') str :: Int
    rest = drop 1 $ dropWhile (/=' ') str
    content =
      if rest !! 0 == '\"'
        then [Right $ [rest !! 1]]
        else 
          let
            parts = words rest
            a = takeWhile (/="|") parts
            b = drop 1 $ dropWhile (/="|") parts
          in
            [ Left $ map read $ a
            , Left $ map read $ b ]

-- ex. [["ab", "a"], ["b"]]     ->   ["abb", "ab"]
combinations :: [[String]] -> [String]
combinations []     = []
combinations [head] = head
combinations list   =
  let
    rest = combinations $ tail list
  in
    concat $ map (\start -> map (start ++) rest) $ head list

updateRuleIfPossible :: RuleMap -> RuleContent -> [RuleContent]
updateRuleIfPossible rules (Right str) = [Right str]
updateRuleIfPossible rules (Left list) =
  let
    matchedRules = mapMaybe (\id -> Map.lookup id rules) list
    matchedStringRules = map rights matchedRules
  in
    if all (\(a,b) -> length a == length b) $ zip matchedRules matchedStringRules
      then  ( map Right $ combinations matchedStringRules )
      else [Left list]

goThroughRules :: RuleMap -> RuleMap
goThroughRules rules =
  Map.map (concat . (map (updateRuleIfPossible rules))) rules

runUntilDoesNotChange :: Eq a => (a -> a) -> a -> a
runUntilDoesNotChange f val =
  let
    newVal = f val
  in
    if newVal == val
      then val
      else runUntilDoesNotChange f newVal

matchesRule :: [RuleContent] -> String -> Bool
matchesRule rules str = any ((==str) . (fromRight "")) rules

fromRight :: b -> Either a b -> b
fromRight def (Left  _) = def
fromRight _   (Right b) = b

beginsWith :: String -> String -> Maybe String
beginsWith str match =
  let
    l    = length match
    comp = take l str
  in
    if match == comp
      then Just $ drop l str
      else Nothing

endsWith :: String -> String -> Maybe String
endsWith str match = maybe Nothing (Just . reverse) (beginsWith (reverse str) (reverse match))

isOkA :: [String] -> String -> Bool
isOkA matchA ""     = True
isOkA matchA string =
  any (
        \a ->
          let
            rest = beginsWith string a
          in
            if isJust rest
              then isOkA matchA (fromJust rest)
              else False
      ) matchA

isOkB :: [String] -> [String] -> String -> Bool
isOkB matchA matchB ""     = True
isOkB matchA matchB string =
  any (
        \a ->
          any (
            \b ->
              let
                restA = beginsWith string a
                restB = maybe Nothing (\restA -> endsWith restA b) restA
              in
                if (isJust restA && isJust restB)
                  then isOkB matchA matchB (fromJust restB)
                  else False
          ) matchB
      ) matchA

isOk :: [String] -> [String] -> String -> Bool
isOk matchA matchB string =
  or  [ isOkA matchA (take i string) && isOkB matchA matchB (drop i string)
      | i <- [2..(length string - 2)]
      ] 

main :: IO ()
main = do
  input <- getInput
  let rules = Map.fromList $ map parseRule $ takeWhile (/="") input
  let strings = drop 1 $ dropWhile (/="") input

  let parsedRules = runUntilDoesNotChange goThroughRules rules

  -- A
  print $ length $ filter (matchesRule $ (parsedRules Map.! 0)) strings

  -- B
  let match42 = map (fromRight "") $ parsedRules Map.! 42
  let match31 = map (fromRight "") $ parsedRules Map.! 31

  print $ length $ filter (isOk match42 match31) strings
