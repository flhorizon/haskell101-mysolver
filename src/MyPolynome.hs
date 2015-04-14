
module MyPolynome
 where

import Data.Complex
import Data.List

infixl 1 :*^:
data Monome = Monome Float Int | Float :*^: Int
 deriving (Ord, Eq, Show)

type Polynome = [Monome] 

newtype Equation = Equation { getEquation :: (Polynome, Polynome) }
 deriving (Ord, Eq)

{-
-- Pretty Monome and [Monome] (aka Polynome) Show.
instance Show Monome where
	showsPrec prec (Monome c p) = showsPrec prec (c :*^: p)
	showsPrec _ (c :*^: p) = shows c . (" * X^" ++) . shows p
	showList []  = ("{Empty set}" ++)
	showList (mn:[]) = shows mn
	showList (m1:(c :*^: p):ms) = showList (m1:(Monome c p):ms)
	showList (m1:(m2@(Monome c2 p2)):ms) = shows m1 . signBridge . next
	 where
	  signBridge
	   | c2 < 0 = (" - " ++)
	   | otherwise = (" + " ++)
	  next
	   | c2 < 0 = showList ((Monome (-c2) p2):ms)
	   | otherwise = showList (m2:ms)
-}

instance Show Equation where
	showsPrec _ (Equation (pl, pr)) = memb pl . showString " = " . memb pr
	 where
	  memb [] = ('0':)
	  memb p  = shows p

--instance Read Monome where
--	readPrec


mbrCoeff :: Monome -> Float
mbrCoeff ( c :*^: _ ) = c
mbrCoeff ( Monome c _ ) = c

mbrPower :: Monome -> Int
mbrPower ( _ :*^: p ) = p
mbrPower ( Monome _ p ) = p


showsComplex :: (Ord c, Num c, Show c, RealFloat c) => Complex c -> ShowS
showsComplex c = shows a . img
	where
	 a = realPart c
	 img
	  | b < 0 = (" - " ++) . shows (-b) . ('i':)
	  | b > 0 = (" + " ++) . shows b . ('i':)
	  | b == 0 = showString ""
	  where
	   b = imagPart c


-- Sums adjacent Monomes of equal power.
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
	  combiMbr = Monome (c1 + c2) p1


-- Tests members' power.
-- True if it can be written aX^2 + bX + c without cutting Monomes out.
isQuadratic :: Polynome -> Bool
isQuadratic poly
	| length ( poly ) == 0 = True
	| pow >= 0 && pow <= 2 = True && isQuadratic ( tail poly ) 
	| otherwise = False
	 where
	  pow = mbrPower ( head poly )

sortPolynome :: Polynome -> Polynome
sortPolynome = sortBy (\a b -> let comparePower = mbrPower ( a ) `compare` mbrPower ( b )
				  in if (comparePower /= EQ)
					then comparePower
					else mbrCoeff ( a ) `compare` mbrCoeff ( b )
					)

-- Make it aX^2 + bX + c ; cut the garbage, insert missing Monomes (power 0->2).
-- Resulting Polynome is in the form (m2:m1:m0:[])
canonicalQuadratic :: Polynome -> Polynome 
canonicalQuadratic poly = smashedPoly
	where
	 cleanedPoly = filter (\m -> mbrPower ( m ) >= 0 && mbrPower ( m ) <= 2 ) poly
	 enrichedPoly = (Monome 0 0):(Monome 0 1):(Monome 0 2):cleanedPoly
	 smashedPoly = polynomeSmash $ sortPolynome enrichedPoly


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

yankLeft :: Equation -> Equation
yankLeft eq@( Equation (_, []) ) = eq
yankLeft (Equation (pl, pr)) = Equation (sortPolynome $ pl ++ pr, [])

-- Idea : compare 0 order monomes both side
-- absurdity :: Equation -> Maybe Equation
-- absurdity
