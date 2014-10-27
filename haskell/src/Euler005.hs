module Euler005 (euler5) where

import Data.List 

euler5 :: Int
euler5 = case find (evenlyDivisible [20,19..11]) [1..] of 
	Just x -> x
	Nothing -> error "aaa"

evenlyDivisible :: [Int] -> Int -> Bool
evenlyDivisible divisors x = all (\d -> x `mod` d == 0) divisors