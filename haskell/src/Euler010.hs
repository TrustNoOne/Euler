module Euler010 (euler10) where

import Data.Numbers.Primes

euler10 :: Int
euler10 = sum $ takeWhile (<2000000) primes