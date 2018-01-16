module Main where

main :: IO ()
main = do
  putStrLn "aproximation of pi"
  mapM_ (putStrLn . show) $ testPiSeq
  return ()

-- --------------------
--
-- make a series from a sequence

toSeries :: Num a => [a] -> [a]
toSeries = scanl (+) 0

-- aproximate pi

piSeq :: [Double]
piSeq = toSeries $ zipWith3 f3 sign plus2 times3
  where
    f3 s p t =
      sqrt12 * s * (1 / (p * t))

    sqrt12 = sqrt 12

    sign, plus2, times3 :: Num a => [a]
    sign   = iterate negate 1
    plus2  = iterate (+2) 1
    times3 = iterate (*3) 1

testPiSeq, testPiDiff :: [Double]
testPiSeq  = take 32 $ piSeq
testPiDiff = take 32 $ map (\x -> x - pi) $ piSeq

-- --------------------
