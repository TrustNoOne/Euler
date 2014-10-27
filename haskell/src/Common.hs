module Common where

sliding :: Int -> [b] -> [[b]]
sliding _ []     = []
sliding n l@(_:xs) = if length l >= n 
                        then (take n l) : sliding n xs
                        else []