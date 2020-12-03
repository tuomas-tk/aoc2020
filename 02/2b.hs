type Divided = (Int, Int, Char, String)


divided :: String -> Maybe Divided
divided str =
    Just (read a :: Int, read b :: Int, letter, word)
  where
    (a, (_:rest1)) = span (/= '-') str
    (b, (_:rest2)) = span (/= ' ') rest1
    letter         = head rest2
    word           = drop 3 rest2

correctCharAtIndex :: String -> Char -> Int -> Bool
correctCharAtIndex word char i =
    word !! (i-1) == char

isValidDivided :: Divided -> Bool
isValidDivided (a, b, letter, word) =
    length
      (filter (correctCharAtIndex word letter) [a, b])
    == 1

isValidString :: String -> Bool
isValidString str = maybe False isValidDivided (divided str)

main :: IO ()
main = do
  input <- readFile "2.in"
  let inputs = lines input
  print (length (filter isValidString inputs))
