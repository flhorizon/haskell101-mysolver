
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)

import Data.DList (toList)
import Control.Monad (join)
import Control.Monad.Trans.Writer (execWriter)

import Data.MyPolynomial.Type
import Data.MyPolynomial.Parser

import Solve


usage :: () -> IO Bool
usage _ = hPutStrLn stderr "Usage: ./mySolver <equation>"
		>> return False


processArgs :: IO (Either () String)
processArgs = do
	args <- getArgs
	if (length args /= 1)
		then return $ Left ()
		else return $ Right (head args)


doExit :: Bool -> IO ()
doExit True = exitSuccess
doExit _ = exitFailure

main :: IO ()
main = do
 	let proceed a = do
 	    	let	wrout = execWriter ( verboseSolve $ ( read  a :: Equation ) )
 	    	  in  putStrLn ( (toList . join) wrout ) >> return True
 	  in processArgs
	  	>>= either usage proceed
		>>= doExit
