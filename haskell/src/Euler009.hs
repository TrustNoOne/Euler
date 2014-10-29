module Euler009 (euler9) where

euler9 :: Int
euler9 = round $ head [ a * b * c | 
                            a <- [2..999], 
                            b <- [a+1..999],
                            let c = sqrt (a^2 + b^2),
                            a + b + c == 1000] 