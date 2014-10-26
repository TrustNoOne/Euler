module Euler004 (euler4) where

euler4 :: Int
euler4 = maximum [x*y | x <- [100..999], y <- [x..999], isPalindrome (x*y)]

isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)