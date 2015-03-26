--{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module MyPolynome
	where

import Data.Complex
import Data.List

infix 5 :*^:
data Member = Member Float Int | Float :*^: Int deriving (Ord, Eq)

type Polynome = [Member] 

-- Pretty Member and [Member] (aka Polynome) Show.
instance Show Member where
	show (Member c p) = show (c :*^: p)
	show (c :*^: p) = ('(':) . shows c . (" * X ^ " ++) . shows p $ ")" 
	showList = goDeeper
		where
			goDeeper []  = ("" ++)
			goDeeper (mn:[]) = shows mn
			goDeeper (m1:(m2@(Member c2 p2)):ms) = shows m1 . signBridge . next
				where
					signBridge
						| c2 < 0 = (" - " ++)
						| otherwise = (" + " ++)
					next
						| c2 < 0 = goDeeper ((Member (-c2) p2):ms)
						| otherwise = goDeeper (m2:ms)


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
-- Resulting Polynome is in the form (m2:m1:m0:[])
canonicalQuadratic :: Polynome -> Polynome 
canonicalQuadratic poly = smashedPoly
	where
		cleanedPoly = filter (\m -> mbrPower ( m ) >= 0 && mbrPower ( m ) <= 2 ) poly
		enrichedPoly = (Member 0 2):(Member 0 1):(Member 0 0):cleanedPoly
		sortedPoly = sortBy (\a b -> mbrPower ( b ) `compare` mbrPower ( a ) ) enrichedPoly
		smashedPoly = polynomeSmash sortedPoly


discriminantQuadratic :: (Num n) => (n, n, n) -> n
discriminantQuadratic (a, b, c) = (b ^ 2 - 4 * a * c)


roots :: (Float, Float, Float) -> (Complex Float, Complex Float)
roots (a, b, c)
	| delta < 0 = (cr1, cr2)
	| delta == 0 = (rr0, rr0)
	| delta > 0 = (rr1, rr2)
	where
		delta = discriminantQuadratic (a, b, c)
		cr1 = (-b / (2 * a)) :+ (sqrt(-delta) / (2 * a))
		cr2 = (-b / (2 * a)) :+ (-sqrt(-delta) / (2 * a))
		rr1 = ((-b - sqrt(delta)) / (2 * a)) :+ 0
		rr2 = ((-b + sqrt(delta)) / (2 * a)) :+ 0
		rr0 = rr1

-- Enforce canonical quadratic form and solve .
solveQuadratic :: Polynome -> (Complex Float, Complex Float)
solveQuadratic supposedQuadratic = 
	let	canonPoly@(m2:m1:m0:[]) = canonicalQuadratic ( supposedQuadratic );
			c2 = mbrCoeff m2;
			c1 = mbrCoeff m1;
			c0 = mbrCoeff m0
			in roots (c2, c1, c0)
