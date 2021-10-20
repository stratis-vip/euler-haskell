module Problem2 where

{-
Each new term in the Fibonacci sequence is generated by adding the previous 
two terms. By starting with 1 and 2, the first 10 terms will be:

                            1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed 
four million, find the sum of the even-valued terms.
-}

fib n = go n (0,1)
  where
    go n (a, b) | n == 0    = a
                | otherwise = go (n-1) (b, a+b)

solution limit  = sum . takeWhile (<limit) . filter even . map fib $ [1..]

main = do
   print ( solution  4000000)