
module Data.MyPolynomial.Print
	where

import Data.MyPolynomial.Type
import Data.Complex (realPart, imagPart, Complex)
import Data.List (sort)
import Data.IntMap.Lazy

---------------------------------------------------------------------------
----  Polynomial / Monomial pretty print
---------------------------------------------------------------------------

prettyMonomialS :: Monomial -> ShowS
prettyMonomialS (0 :*^: _) = showString ""
prettyMonomialS (c :*^: 0) = shows c
prettyMonomialS (c :*^: 1) = shows c . showString " * x"
prettyMonomialS (c :*^: p) = prettyMonomialS (c :*^: 1) . ('^':) . shows p


prettyPolynomialS :: Polynomial -> ShowS
prettyPolynomialS []  = ('0':)
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

prettyPolynomialM :: IntMap Float -> String
prettyPolynomialM mmap = ( prettyPolynomial . sort ) $ consUp (smallest 0 mmap) mmap []
  where
    smallest k map = case lookupLT k map of { Nothing -> k;	Just (k, _) -> smallest k map; }
    consUp k map ls = case lookupGE k map of	Nothing -> ls
    						Just (kn, 0) -> consUp (kn + 1) map ls
						Just (kn, v) -> consUp (kn + 1) map ((v :*^: kn):ls)

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

prettyComplex :: (Ord c, Num c, Show c, RealFloat c) => Complex c -> String
prettyComplex c = prettyComplexS c ""

