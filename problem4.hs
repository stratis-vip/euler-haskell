module Problem4 where

{-
A palindromic number reads the same both ways. The largest palindrome made from 
the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers.
-}

isPalindrome :: Integer -> Bool
isPalindrome x = show x == (foldl (flip (:)) [] $ show x)


solution = head . 
  take 1 $ 
  [x*y 
  | x <- reverse [900..999], y <- reverse [900..999]
  , isPalindrome (x * y) ]

main = do
   print ( solution )
