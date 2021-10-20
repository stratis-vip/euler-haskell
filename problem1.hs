module Problem1 where

{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we 
get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}
fib :: Int -> Integer
fib n = go n (0,1)
  where
    go n (a, b) | n == 0    = a
                | otherwise = go (n-1) (b, a+b)

solution limit = sum [x | x<- [1..limit], mod x 3 == 0 ||  mod x 5 == 0 ]
main = do
   print (solution  999)
