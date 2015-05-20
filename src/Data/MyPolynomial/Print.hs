
module Data.MyPolynomial.Print
	where

import Data.MyPolynomial.Type
import Data.Complex

---------------------------------------------------------------------------
----  Polynomial / Monomial pretty print
---------------------------------------------------------------------------

prettyMonomialS :: Monomial -> ShowS
prettyMonomialS (c :*^: p) = shows c . (" * X^" ++) . shows p

prettyPolynomialS :: Polynomial -> ShowS
prettyPolynomialS []  = ("{Empty set}" ++)
prettyPolynomialS (mn:[]) = prettyMonomialS mn
prettyPolynomialS (m1:(m2@(c2 :*^: p2)):ms) = prettyMonomialS m1 . signBridge . next
 where
  signBridge
   | c2 < 0 = (" - " ++)
   | otherwise = (" + " ++)
  next
   | c2 < 0 = prettyPolynomialS (((-c2) :*^: p2):ms)
   | otherwise = prettyPolynomialS (m2:ms)

prettyMonomial :: Monomial -> String
prettyMonomial m = prettyMonomialS m []

prettyPolynomial :: Polynomial -> String
prettyPolynomial p = prettyPolynomialS p []


---------------------------------------------------------------------------
----  Equation pretty print
---------------------------------------------------------------------------

prettyEquationS :: Equation -> ShowS
prettyEquationS (Eq (pl, pr)) = memb pl . showString " = " . memb pr
 where
  memb [] = ('0':)
  memb p  = prettyPolynomialS p

prettyEquation :: Equation -> String
prettyEquation eq = prettyEquationS eq []


---------------------------------------------------------------------------
----  Complex number pretty print
---------------------------------------------------------------------------

prettyComplexS :: (Ord c, Num c, Show c, RealFloat c) => Complex c -> ShowS
prettyComplexS c = shows a . img
	where
	 a = realPart c
	 img
	  | b < 0 = (" - " ++) . shows (-b) . ('i':)
	  | b > 0 = (" + " ++) . shows b . ('i':)
	  | b == 0 = showString ""
	  where
	   b = imagPart c
