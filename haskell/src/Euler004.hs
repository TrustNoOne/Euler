module Euler004 (euler4) where

euler4 :: Integer
euler4 = maximum [x*y | x <- [100..999], y <- [x..999], isPalindrome (x*y)]

isPalindrome x = show x == reverse (show x)