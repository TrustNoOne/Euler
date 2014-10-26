module Euler002 (solution) where

solution :: IO Integer
solution = do return $ sum $ takeWhile (<4000000) evenFibs
	where 
		fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)
		evenFibs = filter (\x -> x `mod` 2 == 0) fibonacci
