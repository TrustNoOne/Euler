module Euler003 (euler3) where

import Data.Numbers.Primes

euler3 :: Int
euler3 = maximum $ primeFactors 600851475143 