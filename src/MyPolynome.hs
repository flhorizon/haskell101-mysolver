
module MyPolynom where

import Data.Complex
import Data.List


data Member = Member Float Int deriving (Show)
type Polynome = [Member] 

mbrCoeff :: Member -> Float
mbrCoeff ( Member c _ ) = c

mbrPower :: Member -> Int
mbrPower ( Member _ p ) = p

showComplex :: (Ord c, Num c, Show c, RealFloat c) => Complex c -> [Char]
showComplex c = show a ++ img
	where
		a = realPart c
		img
			| b < 0 = " - " ++ show (-b) ++ "i"
			| b > 0 = " + " ++ show b ++ "i"
			| b == 0 = ""
				where
					b = imagPart c


-- Sums adjacent Members of equal power.
polynomeSmash :: Polynome -> Polynome
polynomeSmash [] = []
polynomeSmash poly@(m1:[]) = poly
polynomeSmash (m1:m2:mn)
	| p1 == p2 = polynomeSmash (combiMbr:mn)
	| otherwise = m1:polynomeSmash (m2:mn)
		where
			p1 = mbrPower m1
			p2 = mbrPower m2
			c1 = mbrCoeff m1
			c2 = mbrCoeff m2
			combiMbr = Member (c1 + c2) p1


-- Tests members' power.
-- True if it can be written aX^2 + bX + c without cutting Members out.
isQuadratic :: Polynome -> Bool
isQuadratic poly
	|	length ( poly ) == 0 = True
	| pow >= 0 && pow <= 2 = True && isQuadratic ( tail poly ) 
	| otherwise = False
		where
			pow = mbrPower ( head poly )


-- Make it aX^2 + bX + c ; cut the garbage, insert missing Members (power 0->2).
canonicalQuadratic :: Polynome -> Polynome 
canonicalQuadratic poly = smashedPoly
	where
		cleanedPoly = filter (\m -> mbrPower ( m ) >= 0 && mbrPower ( m ) <= 2 ) poly
		enrichedPoly = (Member 0 2):(Member 0 1):(Member 0 0):cleanedPoly
		sortedPoly = sortBy (\a b -> mbrPower ( b ) `compare` mbrPower ( a ) ) enrichedPoly
		smashedPoly = polynomeSmash sortedPoly


discriminantQuadratic :: (Num n) => (n, n, n) -> n
discriminantQuadratic (a, b, c) = (b ^ 2 - 4 * a * c)


realRoots :: (Float, Float, Float) -> (Float, Float)
realRoots (a, b, c) =
	let delta = discriminantQuadratic (a, b, c)
		in let	x1 = (-b - sqrt(delta)) / (2 * a);
						x2 = (-b + sqrt(delta)) / (2 * a)
						in (x1, x2)


complexRoots :: (Float, Float, Float) -> (Complex Float, Complex Float)
complexRoots (a, b, c) = 
	let delta = discriminantQuadratic (a, b, c)
			in let	x1 = (-b / (2 * a)) :+ (sqrt(delta) / (2 * a));
							x2 = (-b / (2 * a)) :+ ((-sqrt(delta) / (2 * a)))
							in (x1, x2)


solveQuadratic :: Polynome -> (Complex Float, Complex Float)
solveQuadratic _ = (5, 1)

