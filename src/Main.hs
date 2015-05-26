
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Data.DList (toList)
import Control.Monad (join)
import Control.Monad.Writer (execWriter)

import Data.MyPolynomial.Type

import Solve

processArgs :: IO (Either () String)
processArgs = do
	args <- getArgs
	if (length args /= 1)
		then return $ Left ()
		else return $ Right (head args)

main :: IO ()
main = do
 	let error _ = hPutStrLn stderr "Usage: ./mySolver <quadratic equation>"
 	    proceed a = do
 	    	let	wrout = execWriter ( verboseSolve $ ( read  a :: Equation ) )
 	    	  in	putStrLn $ (toList . join) wrout
 	  in processArgs >>= either error proceed
