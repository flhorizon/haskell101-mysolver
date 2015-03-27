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
		 m1 = Monome (-3.5) 2
		 m2 = Monome 1 1
		 m2' = Monome (-0.5) 1
		 m3 = Monome (1/3) 0

--	print $ isQuadratic $ (Monome (-1) (-1)):(Monome (-1) (1)):[]

	let lolPoly = (Monome 666 (-69)):concat( [poly, canonicalQuadratic ( [] )] )
--	putStrLn $ MyPolynome.showList lolPoly $ []
	print lolPoly
	let roots = solveQuadratic lolPoly
			in putStrLn $ showSolution roots
--	let inp = read "(5.0 * X ^ 1)"
	return ()
--	print $ read "(0.0 * X ^ 0)"
