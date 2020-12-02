type Divided = (Int, Int, Char, String)


countLetter :: String -> Char -> Int
countLetter str chr = length (filter (==chr) str)

between :: Int -> Int -> Int -> Bool
between min max n = n >= min && n <= max


divided :: String -> Maybe Divided
divided str =
    Just (read min :: Int, read max :: Int, letter, word)
  where
    (min, (_:rest1)) = span (/= '-') str
    (max, (_:rest2)) = span (/= ' ') rest1
    letter           = head rest2
    word             = drop 3 rest2

isValidDivided :: Divided -> Bool
isValidDivided (min, max, letter, word) =
    between min max (countLetter word letter)

isValidString :: String -> Bool
isValidString str = maybe False isValidDivided (divided str)

main :: IO ()
main = do
  input <- readFile "2.in"
  let inputs = lines input
  print (length (filter isValidString inputs))
