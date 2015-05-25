
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Data.DList as D
import Control.Monad.Writer

import Data.MyPolynomial
import Data.MyPolynomial.Parser
import Data.MyPolynomial.Print

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
	    	let	f = (\a b -> a ++ b)
			wrout = execWriter (verboseSolve $ readEquation a)
			output = (D.foldr f "") wrout
	    	  in putStrLn output
	  in processArgs >>= either error proceed
