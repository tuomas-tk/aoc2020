-- 04 --

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

splitToPassports :: [String] -> [[String]]
splitToPassports [] = []
splitToPassports list =
    ( concat (map words first) ) : splitToPassports(drop 1 rest)
  where
    (first, rest) = break (=="") list

listContains :: [String] -> String -> Bool
listContains list key = foldr (||) False (map (((==) key) . (take 3)) list)

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValidPassport :: [String] -> Bool
isValidPassport fields = all (listContains fields) requiredFields

main :: IO ()
main = do
  input <- getInput
  let passports = splitToPassports input
  print (length (filter isValidPassport passports))
