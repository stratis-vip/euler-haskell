module Problem3 where

{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

dividers:: Integer -> [Integer]
dividers y = filter (\x -> mod y x == 0) $ [2..round (sqrt $ fromInteger y)]

al:: [Integer] -> [Integer] -> [Integer]
al (x:xs) start 
    | xs == []  = start
    | otherwise = start ++ filter(\y -> mod y x /=0) (al xs (start ++ (head xs):[] )) 

solution limit  = maximum $ al (divs) [head divs]
  where divs = dividers limit

main = do

   print ( solution 600851475143 )
