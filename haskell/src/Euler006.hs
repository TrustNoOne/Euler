module Euler006 (euler6) where

euler6 :: Int
euler6 = (sumn 100) ^ 2 - sum (map (^2) [1..100])
 
sumn :: Int -> Int
sumn x = (x * (x + 1)) `div` 2