
import MyPolynome
import Data.Complex

showSolution :: (Complex Float, Complex Float) -> [Char]
showSolution (c1, c2)
	| c1 == c2 = "Dual root is " ++ showComplex ( c1 )
	| imagPart ( c1 ) /= 0 || imagPart ( c2 ) /= 0 = "Two complex roots : " ++ showComplex (c1 ) ++ ",\t" ++ showComplex ( c2 )
	| otherwise = "Two real roots : " ++ showComplex (c1 ) ++ ",\t" ++ showComplex ( c2 )


main :: IO ()
main = do	
	let poly = [m1, m2, m2', m3]
		where
			m1 = Member (-3.5) 2;
			m2 = Member 1 1;
			m2' = Member (-0.5) 1;
			m3 = Member (1/3) 0

--	print $ isQuadratic $ (Member (-1) (-1)):(Member (-1) (1)):[]

	let lolPoly = canonicalQuadratic $ (Member 666 (-69)):concat( [poly, canonicalQuadratic ( [] )] )
	print ( lolPoly )
	let roots = solveQuadratic poly
			in putStrLn $ showSolution roots
