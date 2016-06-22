import Control.Parallel.Strategies

fib :: Int -> Int
fib x 
  | x == 0    = 0
  | x == 1    = 1
  | otherwise = fib (x - 2) + fib (x - 1)
	
fibShort :: Int -> Int
fibShort n = let fibs = iterate (\(a, b) -> (b,a+b)) (0, 1) 
             in head $ fmap (\x -> fst x + snd x) $ drop (n-1) $ take n $ fibs

