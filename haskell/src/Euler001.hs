module Euler001 (solution) where

import Control.Concurrent

solution :: IO Integer
solution = do return $ sum [1..100000000]

