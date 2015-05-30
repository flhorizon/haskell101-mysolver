
module Data.MyPolynomial.Print
	where

import Data.MyPolynomial.Type
import Data.Complex (realPart, imagPart, Complex)
import Data.List (sort)
import Data.IntMap.Lazy (lookupLT, lookupGE, IntMap)

 -- `procelain' functions are intended for canonical printing :
 -- i.e. Show instances, i.e. serialization, i.e. Read reciprocal
 
 -- `pretty' functions are intended for pretty printing only :
 -- i.e. absolutly no consistent Read <=> Show reciprocity.
 
 -- `prints' functions are general purpose printing functions;
 -- they either use `pretty' or `porcelain' to generate output.
 -- The 3rd person-like terminal 's' is a general Haskell convention
 -- for functions using a provided interface to do a specific job.
 
 -- S suffixed functions return a String difference list (ShowS)
 -- for efficient arbitrary concatenation.
 
 -- M suffixed functions take a polynomial represented by
 -- an IntMap Float instance.

---------------------------------------------------------------------------
----  Abstract printers
---------------------------------------------------------------------------

printsPolynomialS :: (Monomial -> ShowS) -> Polynomial -> ShowS
printsPolynomialS _ []  = ('0':)
printsPolynomialS pfS (mn:[]) = pfS mn
printsPolynomialS pfS (m1:(m2@(c2 :*^: p2)):ms) = pfS m1 . signBridge . next
 where
  signBridge
   | c2 < 0 = (" - " ++)
   | otherwise = (" + " ++)
  next
   | c2 < 0 = printsPolynomialS pfS (((-c2) :*^: p2):ms)
   | otherwise = printsPolynomialS pfS (m2:ms)
   
   
printsPolynomialSM :: (Polynomial -> ShowS) -> IntMap Float -> ShowS
printsPolynomialSM pfS mmap = ( pfS . sort ) $ consUp (smallest 0 mmap) mmap ([]++) []
  where
    smallest k map = case lookupLT k map of { Nothing -> k;	Just (k, _) -> smallest k map; }
    consUp k map dl = case lookupGE k map of	Nothing -> dl
						Just (kn, v) -> consUp (kn + 1) map (((v :*^: kn):) . dl)
   
printsEquationS :: (Polynomial -> ShowS) -> Equation -> ShowS
printsEquationS pfS (Eq (l, r)) = memb l . showString " = " . memb r
 where
  memb [] = ('0':)
  memb p  = pfS p
  
  

---------------------------------------------------------------------------
----  Canonical printing ( Read <=> Show )
---------------------------------------------------------------------------

porcelainMonomialS :: Monomial -> ShowS
porcelainMonomialS (c :*^: p) = shows c . showString " * X^" . shows p

porcelainPolynomialS :: Polynomial -> ShowS
porcelainPolynomialS = printsPolynomialS porcelainMonomialS

porcelainEquationS :: Equation -> ShowS
porcelainEquationS = printsEquationS porcelainPolynomialS

porcelainPolynomialSM :: IntMap Float -> ShowS
porcelainPolynomialSM = printsPolynomialSM porcelainPolynomialS



---------------------------------------------------------------------------
----  Pretty printing
---------------------------------------------------------------------------

prettyMonomialS :: Monomial -> ShowS
prettyMonomialS (0 :*^: _) = showString ""
prettyMonomialS (c :*^: 0) = shows c
prettyMonomialS (c :*^: 1) = shows c . showString " X"
prettyMonomialS (c :*^: p) = prettyMonomialS (c :*^: 1) . ('^':) . shows p

prettyMonomial :: Monomial -> String
prettyMonomial mn =  prettyMonomialS mn ""



prettyPolynomialS :: Polynomial -> ShowS
prettyPolynomialS = ( printsPolynomialS prettyMonomialS ) . purgeHollow
  where purgeHollow = filter ( \(c :*^: _) -> c /= 0 )

prettyPolynomial :: Polynomial -> String
prettyPolynomial p = prettyPolynomialS p ""



prettyPolynomialSM :: IntMap Float -> ShowS
prettyPolynomialSM = printsPolynomialSM prettyPolynomialS

prettyPolynomialM :: IntMap Float -> String
prettyPolynomialM m = prettyPolynomialSM m []




prettyEquationS :: Equation -> ShowS
prettyEquationS = printsEquationS (printsPolynomialS porcelainMonomialS)

prettyEquation :: Equation -> String
prettyEquation eq = prettyEquationS eq []




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


---------------------------------------------------------------------------
----  Show instances
---------------------------------------------------------------------------


instance Show Monomial where
  showsPrec _ = porcelainMonomialS
  showList = porcelainPolynomialS

instance Show Equation where
  showsPrec _ = porcelainEquationS

