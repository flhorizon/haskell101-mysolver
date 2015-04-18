import MyPolynomial
import MyPolynomial.Print
import Data.Complex

import Data.List

prettyPrintSolution :: Either ShowS Roots -> String
prettyPrintSolution (Left s) = s []
prettyPrintSolution (Right (c1, c2))
	| c1 == c2 = ("Dual root is \n" ++) . prettyComplexS ( c1 ) $ []
	| imagPart ( c1 ) /= 0 || imagPart ( c2 ) /= 0 = ("Two complex roots :\n" ++) . prettyComplexS (c1 ) . ('\n':) . prettyComplexS ( c2 ) $ []
	| otherwise = ("Two real roots :\n" ++) . prettyComplexS (c1 ) . ('\n':) . prettyComplexS ( c2 ) $ []


-- The given polynomial is assumed to be sorted.
logDegree :: Polynomial -> ShowS
logDegree [] = logDegree [0 :*^: 0]
logDegree p = showString "Equation degree is " . shows (mbrPower $ last p) . showString ".\n"

-- TODO : Reduce phase. Remove canonical enforcement for out-of-bounds orders.

main :: IO ()
main = do	
	let eq@(Equation (poly,rpol)) = Equation ([m1, m2, m3], [ma, mb])
		where
		 m1 = (3.5) :*^: 2
		 m2 = 1 :*^: 1
		 m3 = (1/3) :*^: 0
		 ma = (-1) :*^: 1
		 mb = (0) :*^: 1


	putStrLn $ prettyEquation eq
	putStrLn $ prettyEquation $ yankLeft eq

	let lolPoly = canonicalQuadratic $ (666 :*^: (-69)):concat( [poly, canonicalQuadratic ( [] )] )
	putStrLn $ prettyPolynomial lolPoly
	putStrLn $ logDegree (canonicalQuadratic lolPoly) []
	let roots = solveQuadratic lolPoly
			in putStrLn $ prettyPrintSolution ( roots )

	putStrLn $ "Equating Monomials / Equations...." 
	print $ (==) (1 :*^: 2) (1 :*^: 2)
	print $ (==) (1 :*^: 2) (1 :*^: 3)
	print $ (==) ((-1) :*^: 2) (1 :*^: 2)

	putStrLn $ "Ord-ering Monomials / Equations...." 
	print $ compare  (1:*^:2) (1:*^:2)
	print $ compare  (1:*^:2) (1:*^:3)
	print $ compare  (5:*^:2) (1:*^:3)
	print $ compare  (1:*^:4) (1:*^:3)
	print $ compare  (2:*^:3) (1:*^:3)
	print $ (>=)  (2:*^:3) (1:*^:3)
	print $ (>=) (1 :*^: 2) (1 :*^: 2)
	print $ (<=)  (2:*^:3) (1:*^:3)

	putStrLn "Sorted from Ord : Before / After"
	print $ prettyPolynomial $ poly
	print $ prettyPolynomial $ sort poly
