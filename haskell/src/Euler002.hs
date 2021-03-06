module Euler002 (euler2) where

euler2 :: Int
euler2 = sum $ takeWhile (<4000000) evenFibs
	where 
		fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)
		evenFibs = filter (\x -> x `mod` 2 == 0) fibonacci
