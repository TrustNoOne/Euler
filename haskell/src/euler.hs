import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import System.IO (hFlush, stdout)
import System.Environment

import Euler001
import Euler002
import Euler003
import Euler004


problems = [io euler1, io euler2, io euler3, io euler4]

main = do 
	args <- getArgs
	n <- if (null args) then readProblemNumberFromStdin else readProblemNumberFromArgs
	(time, result) <- eval $ problems !! n
	putStrLn $ "Result: " ++ (show result) ++ ". Elapsed time: " ++ (show time)
	
io :: a -> IO a
io x = return x

readProblemNumberFromArgs :: IO Int
readProblemNumberFromArgs = do
	args <- getArgs
	let n = (read $ head args)
	return $ n - 1

readProblemNumberFromStdin :: IO Int
readProblemNumberFromStdin = do
	putStr "Please insert the problem number: "
	hFlush stdout
	line <- getLine
	return $ (read line) - 1

eval :: IO a -> IO (POSIXTime, a)
eval ioa = do
	t1 <- getPOSIXTime
	result <- ioa
	return $! result
	t2 <- getPOSIXTime
	return (t2 - t1, result)