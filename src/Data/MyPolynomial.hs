
module Data.MyPolynomial (
	Monomial ((:*^:))
	, (*^)
	, Equation(Eq)
	, getEq
	, getL
	, getR
	, Polynomial
	, Roots
	, mbrCoeff
	, mbrPower
	, isQuadratic
	, discriminantQuadratic
	, roots
	, condense
	, degree
	, handleNegativePowers
	, getCoeffs
	, absurd
	, divergent
	) where

import Data.Complex (Complex((:+)))
import Data.List
import Control.Applicative ((<$>), (<*>))
import Data.MyPolynomial.Type
import Data.MyPolynomial.Parser
import Control.Monad.State


type ComplexF = Complex Float
type Roots = (ComplexF, ComplexF)
type NonRoots = [ComplexF]

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

getCoeffs :: Polynomial -> (Float, Float, Float)
getCoeffs p	| [m0, m1, m2] <- p = (mbrCoeff m0, mbrCoeff m1, mbrCoeff m2)
		| otherwise = error "Uncondensed or non-quadratic polynomial."


-- Tests members' power.
-- True if it can be written aX^2 + bX + c without cutting Monomials out.
isQuadratic :: Polynomial -> Bool
isQuadratic poly
	| length ( poly ) == 0 = True
	| pow >= 0 && pow <= 2 = True && isQuadratic ( tail poly ) 
	| otherwise = False
	 where
	  pow = mbrPower ( head poly )



discriminantQuadratic :: (Num n) => (n, n, n) -> n
discriminantQuadratic (a, b, c) = (b ^ 2 - 4 * a * c)


roots :: (Float, Float, Float) -> Roots
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

yankLeft :: Equation -> Equation
yankLeft (Eq (lp, rp)) = Eq (sort $ lp ++ (move rp), [zero])
  where
    move [] = []
    move ((c :*^: p):rpn) = ((-c) :*^: p):move(rpn)


condense :: Equation -> Equation
condense eq = let Eq (l, r) = yankLeft eq;
		  l' = polynomeSmash l;
		in Eq (l', r )

degree :: Polynomial -> Int
degree pl = (`highest` pl) (mbrPower $ head pl)
  where
    highest h [] = h
    highest h (m1:mp)	| [] <- mp = mbrPower m1
    			| _ :*^: p <- m1 = highest (max h p) mp


-- Monomial multiplication
infix 5 *^*
(*^*) :: Monomial -> Monomial -> Monomial
c1 :*^: p1 *^* c2 :*^: p2 = (c1 * c2) :*^: (p1 + p2)

infixr 5 /^/
(/^/) :: Monomial -> Monomial -> Monomial
c1 :*^: p1 /^/ c2 :*^: p2 = (c1 * (1.0 / c2)) :*^: (p1 - p2)

-- Monomial sum
infix 4 +^+
(+^+) :: Monomial -> Monomial -> Polynomial
m1@(c1 :*^: p1) +^+ m2@(c2 :*^: p2) | p1 == p2 = [(c1 + c2) :*^: p1]
				    | otherwise = m1:m2:[]

infix 4 -^-
(-^-) :: Monomial -> Monomial -> Polynomial
m1 -^- (c2 :*^: p2) = m1 +^+ ((-c2) :*^: p2)


-- Remove negative power monomes and adds forbidden roots to the State.
handleNegativePowers :: Polynomial -> State NonRoots Polynomial 
handleNegativePowers p = case (findNeg p) of	Nothing -> get >>= (\s -> state (\_ -> (p, s)))
						Just mn -> handleNext mn p
  where
    bundle (a, b) = nub [a, b]
    addForbidden st nr = nub $ st ++ nr

    handleNext :: Monomial -> Polynomial -> State NonRoots Polynomial
    handleNext mx pl = do
    	get >>= (\s -> put $ addForbidden s (badRoots mx))
	handleNegativePowers ( applyRcp mx (delete mx pl) )

    applyRcp :: Monomial -> Polynomial -> Polynomial
    applyRcp (_ :*^: np) pl = (*^* 1.0 :*^: (-np)) <$> pl

    badRoots :: Monomial -> [ComplexF]
    badRoots (c :*^: p) | (-p) == 0 = bundle $ roots (0, 0, c)
    			| (-p) == 1 = bundle $ roots (0, c, 0)
			| (-p) == 2 = bundle $ roots (c, 0, 0)
			| otherwise = error "Monomial of a higher degree that 2 !"

    findNeg = find (\(_ :*^: p) -> p < 0)


 -- Working on a condensed Equation.
absurd :: Equation -> Bool
absurd eq	| (Eq ([c1 :*^: 0], [c2 :*^: 0])) <- eq = c1 /= c2
		| otherwise = False

 -- Working on a condensed Equation.
divergent :: Equation -> Bool
divergent (Eq (l, r)) = l == r
