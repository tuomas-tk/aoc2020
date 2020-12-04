-- 04 --
import Text.Regex.TDFA

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

data Field = Field {
  key   :: String,
  value :: String
} deriving (Show)

type Passport = [Field]

stringToField :: String -> Field
stringToField str = Field (take 3 str) (drop 4 str)

splitToPassports :: [String] -> [Passport]
splitToPassports [] = []
splitToPassports list =
    (map stringToField ( concat (map (words) first) )) : splitToPassports(drop 1 rest)
  where
    (first, rest) = break (=="") list

startsWith :: String -> String -> Bool
startsWith start str = (take 3 str) == start

isValidField :: String -> Checker -> Field -> Bool
isValidField key checker field@(Field fKey fValue) = (fKey == key) && (checker field)

hasValidField :: Passport -> (String, Checker) -> Bool
hasValidField list (key, checker) = or $ map (isValidField key checker) list


type Checker = Field -> Bool

isBetween :: Int -> Int -> Int -> Bool
isBetween min max n = (n >= min) && (n <= max)

nDigitMinMaxChecker :: Int -> Int -> Int -> Checker
nDigitMinMaxChecker n min max (Field key val) = (val =~ ("^[0-9]{" ++ (show n) ++ "}$") :: Bool)
                                                 && isBetween min max (read val :: Int)

heightChecker :: Checker
heightChecker (Field key val) = ((val =~ "^[0-9]+cm$") && (isBetween 150 193 (read $ reverse $ (drop 2) $ reverse val)))
                                || ((val =~ "^[0-9]+in$") && (isBetween 59 76 (read $ reverse $ (drop 2) $ reverse val)))

hairColorChecker :: Checker
hairColorChecker (Field key val) = val =~ "^#[0-9a-f]{6}$"

eyeColorChecker :: Checker
eyeColorChecker (Field key val) = val =~ "^(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)$"

requiredFields = [ ("byr", nDigitMinMaxChecker 4 1920 2002)
                 , ("iyr", nDigitMinMaxChecker 4 2010 2020)
                 , ("eyr", nDigitMinMaxChecker 4 2020 2030)
                 , ("hgt", heightChecker)
                 , ("hcl", hairColorChecker)
                 , ("ecl", eyeColorChecker)
                 , ("pid", nDigitMinMaxChecker 9 0 999999999)
                 ] :: [( String, Checker )]

isValidPassport :: Passport -> Bool
isValidPassport passport = all (hasValidField passport) requiredFields

main :: IO ()
main = do
  input <- getInput
  let passports = splitToPassports input
  print $ length $ filter isValidPassport passports
