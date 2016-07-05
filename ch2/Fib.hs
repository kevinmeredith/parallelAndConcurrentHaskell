module Main where

import Control.Parallel.Strategies
import Control.Applicative

main :: IO ()
main = putStrLn . show $ spark [1..40]

fib :: Int -> Int
fib x 
 | x <= 1    = 1
 | otherwise = fib (x-1) + fib (x-2)
 

spark :: [Int] -> [Int]
spark = parMap rpar	 fib 

splitIt :: [Int] -> Eval [Int]
splitIt xs = let len = length xs
                 (as, bs) = splitAt (len `div` 2) xs
             in
              do
              	xs <- fibPar as
              	ys <- fibPar bs
              	return $ xs ++ ys
             
fibPar :: [Int] -> Eval [Int]
fibPar []     = return []
fibPar (x:xs) = do
	a  <- rpar $ fib x
	as <- fibPar xs
	return $ a : as