module Gen
where

import System.Environment

main :: IO ()
main
    = do n <- fmap (read . head) $ getArgs
         putStrLn . show $ [ i | i <- [1 .. n - 1]]
         putStrLn . show $ (n::Int)