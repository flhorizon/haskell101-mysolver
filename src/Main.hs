
import System.Environment (getArgs)

import Data.MyPolynomial
import Data.MyPolynomial.Parser
import Data.MyPolynomial.Print

instance Show Monomial where
	showsPrec _ = prettyMonomialS
	showList = prettyPolynomialS

instance Show Equation where
	showsPrec _ = prettyEquationS

main = do
  	let input = "  -55 * X ^  0    "
  	let in4 = (unwords $ take 4 $ repeat input )
  
  
  	print $ readsMonomial input
  	print $ readsMonomial "-1234 * x ^ 3"
  
  	let ls = [ (a *^ 2) | a <- [-2..3] ]
  
  	print ls
  	putStrLn []
  	print $ readsPolynomial $ show ls
  	print $ in4
  
  	let hq = Eq (ls, fst . head $ readsPolynomial in4)
  	print hq
  	putStrLn []
  	print $ fst . head $ readsEquation ( show hq )
  	
  	putStrLn "\nReal now ? \n"
  	l <- getArgs >>= (\(a:as) -> return a)
  	print $ readEquation l
  	print $ readsEquation l
