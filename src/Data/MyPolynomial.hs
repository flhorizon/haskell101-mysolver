
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
	, discriminantQuadratic
	, roots
	, canonify
	, degree
	, handleNegativePowers
	, getABC
	, toMap
	, absurd
	, divergent
	) where

import Data.Complex (Complex((:+)))
import Data.List
import qualified Data.IntMap.Lazy as M
import Control.Monad.Reader
import Control.Monad.State

import Data.MyPolynomial.Type
import Data.MyPolynomial.Parser


type ComplexF = Complex Float
type Roots = (ComplexF, ComplexF)
type NonRoots = [ComplexF]

-- Sums adjacent Monomials of equal power.
squeeze' :: Polynomial -> Polynomial
squeeze' [] = []
squeeze' poly@(m1:[]) = poly
squeeze' (m1:m2:mn)
	| p1 == p2 = squeeze (combiMbr:mn)
	| otherwise = m1:squeeze (m2:mn)
	 where
	  p1 = mbrPower m1
	  p2 = mbrPower m2
	  c1 = mbrCoeff m1
	  c2 = mbrCoeff m2
	  combiMbr = (c1 + c2) :*^: p1


 -- Sums every Monomial of equal power.
squeeze :: Polynomial -> Polynomial
squeeze poly = runReader (doSqueeze poly) (powers poly)
	where
		 -- Build an unique list of represented powers in @pl@
		powers pl = nub $ fmap mbrPower pl

		-- Every @mn@ in @pl@ whose power is equal to the provided @pw@.
		samePower pw pl = partition (\m -> mbrPower m == pw) pl) 

		-- For each the provided power @pw@, partition out bros of power p, sum them, add them back to the other partition.
		foldPower pw pl = let	(bros, others) = samePower pw pl
					brogogo = foldr (\macc (m:_) -> head $ macc +^+ m) (zeroP pw) bros
					in (brogogo:others)

		-- Pseudo type annotation : 
		-- doSequeeze :: Polynomial -> Reader Env@[Power] Polynomial
		doSqueeze pl = do
			e:es <- ask
			let pl' = foldPower e pl
			  in case es of	[] -> return pl'
					_ -> local (\ _:en -> en ) (doStuff pl')

 -- Returns a, b, c coefficients as in the canonical expression aX^2 + bX + c
 -- Returns triple NaN if matching fails.
getABC :: M.IntMap Float -> (Float, Float, Float)
getABC fromList [(0, c), (1, b), (2, a)] = (a, b, c)
getABC _ = (sqrt (-1), sqrt (-1), sqrt (-1))


 -- Takes a condensed quadratic list polynomial; calls error otherwise.
toMap :: Polynomial -> M.IntMap Float
toMap pl = mapIt $ sort $ squeeze pl
	where
	  mapIt [(c0 :*^: p0):(c1 :*^: p1):(c2 :*^: p2)] = M.fromList [(p0, c0), (p1, c1), (p2, c2)]
	  mapIt _ = error "Uncondensed or non-quadratic list polynomial."


 -- Expects a canonical quadratic representation; returns NaN otherwise.
discriminantQuadratic :: M.IntMap Float -> Float
discriminantQuadratic fromList [(0, c), (1, b), (2, a)] = (b ^ 2.0 - 4.0 * a * c)
discriminantQuadratic _ = sqrt (-1)


roots :: IntMap Float -> Roots
roots mmap
	| delta < 0 = (cr1, cr2)
	| delta == 0 = (rr0, rr0)
	| delta > 0 = (rr1, rr2)
	where
	 (a, b, c) = getABC mmap
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


canonify :: Equation -> Equation
canonify (Eq (l,r)) = let l' = ((0 :*^: 0):(0 :*^: 1):(0 :*^: 2):l)
			  Eq (l'', r') = yankLeft $ Eq (l', r)
			  l''' = squeeze l''
			 in Eq (l''', r')


degree :: M.IntMap Float -> Int
degree mmap = withStartingKey 0
	where
	  withStartingKey k = highest k
	  highest k = case M.lookupGE k mmap of { Nothing -> smallest k; Just (k, _) -> k; }
	  smallest k = case M.lookupLE k mmap of { Nothing -> 0; Just (k, _) -> withStartingKey k; }


cutHighZeroes :: Equation -> Equation
cutHighZeroes (Eq (l, r)) = Eq (skim l, skim r)
	where skim = filter (\(c :*^: p) -> c /= 0 || (c == 0 && p == 0))


absurd :: Equation -> Bool
absurd eq	| Eq ([cl :*^: 0], [cr :*^: 0]) <- eq'  = cl /= cr
		| otherwise = False
	where
	  Eq (l, r) = yankLeft eq
	  eq' = cutHighZeroes $ Eq (squeeze l, squeeze r)


divergent :: Equation -> Bool
divergent eq	| Eq ([cl :*^: 0], [cr :*^: 0]) <- eq'  = cl == cr
		| otherwise = False
	where
	  Eq (l, r) = yankLeft eq
	  eq' = cutHighZeroes $ Eq (squeeze l, squeeze r)


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


-- Transforms negative power monomes away and adds forbidden roots to the State.
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
    applyRcp (_ :*^: np) pl = fmap (*^* 1.0 :*^: (-np)) pl

    badRoots :: Monomial -> [ComplexF]
    badRoots (c :*^: p) | (-p) == 0 = bundle $ roots (0, 0, c)
    			| (-p) == 1 = bundle $ roots (0, c, 0)
			| (-p) == 2 = bundle $ roots (c, 0, 0)
			| otherwise = error "Monomial of a higher degree that 2 !"

    findNeg = find (\(_ :*^: p) -> p < 0)


