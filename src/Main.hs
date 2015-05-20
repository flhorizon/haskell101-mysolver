
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Data.MyPolynomial
import Data.MyPolynomial.Parser
import Data.MyPolynomial.Print

 -- instance Show Monomial where
 -- 	showsPrec _ = prettyMonomialS
 -- 	showList = prettyPolynomialS
 -- 
 -- instance Show Equation where
 -- 	showsPrec _ = prettyEquationS

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
		putStrLn $ prettyEquation $ readEquation a
	  in processArgs >>= either error proceed
