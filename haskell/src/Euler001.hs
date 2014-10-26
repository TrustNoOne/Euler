module Euler001 (solution) where

solution :: IO Integer
solution = do 
	return $ sum [x | x <- [1..999], x `mod` 5 == 0 || x `mod` 3 == 0]
