
module MyPolynomial
 where

import Data.Complex
import Data.List

infixl 1 :*^:
data Monomial = Float :*^: Int
  deriving (Eq)

type Polynomial = [Monomial] 

type Roots = (Complex Float, Complex Float)

newtype Equation = Equation { getEquation :: (Polynomial, Polynomial) }

instance Ord Monomial where
  m1@(c1 :*^: p1) `compare` m2@(c2 :*^: p2)
    | m1 == m2 = EQ
    | (p1 < p2) || ((p1 == p2) && (c1 < c2)) = LT
    | (p1 > p2) || ((p1 == p2) && (c1 > c2)) = GT


mbrCoeff :: Monomial -> Float
mbrCoeff ( c :*^: _ ) = c

mbrPower :: Monomial -> Int
mbrPower ( _ :*^: p ) = p


-- Sums adjacent Monomials of equal power.
polynomeSmash :: Polynomial -> Polynomial
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
	  combiMbr = (c1 + c2) :*^: p1


-- Tests members' power.
-- True if it can be written aX^2 + bX + c without cutting Monomials out.
isQuadratic :: Polynomial -> Bool
isQuadratic poly
	| length ( poly ) == 0 = True
	| pow >= 0 && pow <= 2 = True && isQuadratic ( tail poly ) 
	| otherwise = False
	 where
	  pow = mbrPower ( head poly )


-- Make it aX^2 + bX + c ; cut the garbage, insert missing Monomials (power 0->2).
-- Resulting Polynomial is in the form (m2:m1:m0:[])
canonicalQuadratic :: Polynomial -> Polynomial 
canonicalQuadratic poly = smashedPoly
	where
	 cleanedPoly = filter (\m -> mbrPower ( m ) >= 0 && mbrPower ( m ) <= 2 ) poly
	 enrichedPoly = (0 :*^: 0):(0 :*^: 1):(0 :*^: 2):cleanedPoly
	 smashedPoly = polynomeSmash $ sort enrichedPoly


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
solveQuadratic :: Polynomial -> Either ShowS Roots
solveQuadratic supposedQuadratic = 
	let	canonPoly@(m0:m1:m2:[]) = canonicalQuadratic ( supposedQuadratic );
		c2 = mbrCoeff m2;
		c1 = mbrCoeff m1;
		c0 = mbrCoeff m0
		in Right $ roots (c2, c1, c0)

yankLeft :: Equation -> Equation
yankLeft (Equation (lp, rp)) = Equation (sort $ lp ++ (flip rp), [])
  where
    flip [] = []
    flip ((c :*^: p):rpn) = ((-c) :*^: p):flip(rpn)

-- Assume yanked left, sorted, condensed equation
absurdity :: Equation -> Maybe Equation
absurdity eq =
  let (pl, pr) = getEquation eq in
    if (mbrPower ( last pl ) > 0 || pl == pr)
      then
	   Just eq
      else
	   Nothing
