
transform :: Int -> Int -> Int
transform subject value = mod (value * subject) 20201227

findLoopSize :: Int -> Int -> Int
findLoopSize subject result = snd
                            $ head
                            $ filter ((==result) . fst)
                            $ zip (iterate (transform subject) 1) [0..]

runTimes :: (a -> a) -> Int -> a -> a
runTimes _ 0 val = val
runTimes f n val = runTimes f (n-1) (f val)

main :: IO ()
main = do
  let (doorPublicKey, cardPublicKey) = (335121, 363891)
  let cardLoopSize = findLoopSize 7 $ cardPublicKey
  print $ runTimes (transform doorPublicKey) cardLoopSize 1