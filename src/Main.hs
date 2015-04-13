import MyPolynome
import Data.Complex

showSolution :: (Complex Float, Complex Float) -> String
showSolution (c1, c2)
	| c1 == c2 = ("Dual root is \n" ++) . showsComplex ( c1 ) $ []
	| imagPart ( c1 ) /= 0 || imagPart ( c2 ) /= 0 = ("Two complex roots :\n" ++) . showsComplex (c1 ) . ('\n':) . showsComplex ( c2 ) $ []
	| otherwise = ("Two real roots :\n" ++) . showsComplex (c1 ) . ('\n':) . showsComplex ( c2 ) $ []


-- TODO : Reduce phase. Remove canonical enforcement for out-of-bounds orders.

main :: IO ()
main = do	
	let eq@(Equation (poly,rpol)) = Equation ([m1, m2, m3], [ma, mb])
		where
		 m1 = Monome (-3.5) 2
		 m2 = Monome 1 1
		 m3 = Monome (1/3) 0
		 ma = (-1) :*^: 1
		 mb = (0) :*^: 1

--	print $ isQuadratic $ (Monome (-1) (-1)):(Monome (-1) (1)):[]

	print  eq
	print  poly
	print  rpol
	print $ yankLeft eq

	let lolPoly = canonicalQuadratic $ (Monome 666 (-69)):concat( [poly, canonicalQuadratic ( [] )] )
--	putStrLn $ MyPolynome.showList lolPoly $ []
	print lolPoly
	let roots = solveQuadratic lolPoly
			in putStrLn $ showSolution roots
--	let inp = read "(5.0 * X ^ 1)"
	return ()
--	print $ read "(0.0 * X ^ 0)"
