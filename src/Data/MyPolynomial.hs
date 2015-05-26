
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
	, BroadSol(Absurd, Real)
	, Solution(Quadratic, Simple, Broad)
	, solveEquation
	) where

import Data.Complex (Complex((:+)))
import Data.List
import qualified Data.IntMap.Lazy as M
import Control.Monad.Reader
import Control.Monad.State

import Data.MyPolynomial.Type
import Data.MyPolynomial.Parser
import Data.MyPolynomial.Print


type ComplexF = Complex Float
type Roots = (ComplexF, ComplexF)
type NonRoots = [ComplexF]


 -- Sums every Monomial of equal power.
squeeze :: Polynomial -> Polynomial
squeeze poly = runReader (doSqueeze poly) (powers poly)
	where
		 -- Build an unique list of represented powers in @pl@
		powers pl = nub $ fmap mbrPower pl

		-- Every @mn@ in @pl@ whose power is equal to the provided @pw@.
		samePower pw pl = partition (\ (_ :*^: mp) -> mp == pw) pl 

		-- For each the provided power @pw@, partition out bros of power p, sum them, add them back to the other partition.
		foldPower pw pl = let	(bros, others) = samePower pw pl
					brogogo = foldr (\ m macc -> head $ macc +^+ m) (zeroP pw) bros
					in (brogogo:others)

		-- Pseudo type annotation : 
		-- doSequeeze :: Polynomial -> Reader Env@[Power] Polynomial
		doSqueeze pl = do
			e:es <- ask
			let pl' = foldPower e pl
			  in case es of	[] -> return pl'
					_ -> local (\ (_:en) -> en ) (doSqueeze pl')

 -- Returns a, b, c coefficients as in the canonical expression aX^2 + bX + c
getABC :: M.IntMap Float -> (Float, Float, Float)
getABC mmap = (mmap M.! 2, mmap M.! 1, mmap M.! 0)


 -- Takes a condensed quadratic list polynomial; calls error otherwise.
toMap :: Polynomial -> M.IntMap Float
toMap pl = mapIt (squeeze pl) (M.singleton 0 0)
	where
	  mapIt ((c :*^: p):mn) map = mapIt mn (M.insert p c map)
	  mapIt [] map = map

 -- Calls error if the polynomial degree is out of bounds ([0 <-> 2]).
assertMap :: M.IntMap Float -> ()
assertMap mmap = let d = degree mmap
		   in if d > 2 || d < 0
		   	then	error "Uncondensed or non-quadratic list polynomial."
		   	else	()


 -- Expects a canonical quadratic representation
discriminantQuadratic :: M.IntMap Float -> Float
discriminantQuadratic mmap = let  (a, b, c) = getABC mmap
				in (b ^ 2 - 4.0 * a * c)


data BroadSol = Absurd | Real
data Solution = Quadratic Roots | Simple Float | Broad BroadSol

solveEquation :: Equation -> Solution
solveEquation eq = (doSolve . toMap . getL . canonify) eq
  where
    doSolve mmap = let 	a = mmap M.! 2
    			b = mmap M.! 1
			c = mmap M.! 0
		    in dispatch a b c mmap
    quadratic m = Quadratic ( roots m )
    simple m = Simple ( solveDeg1 m )
    broad m = Broad ( broadSolve m )
    dispatch a b c m	| a /= 0 = quadratic m
			| a == 0 && b /= 0 = simple m
			| a == 0 && b == 0 = broad m


broadSolve :: M.IntMap Float -> BroadSol
broadSolve mmap	= case mmap M.! 0 of	0 -> Real
					_ -> Absurd


solveDeg1 :: M.IntMap Float -> Float
solveDeg1 mmap = let	b = mmap M.! 1
			c = mmap M.! 0
		   in (-c) / b


roots :: M.IntMap Float -> Roots
roots mmap
	| delta < 0 = (cr1, cr2)
	| delta == 0 = (rr0, rr0)
	| delta > 0 = (rr1, rr2)
	where
	 (a, b, c) = getABC mmap
	 delta = discriminantQuadratic mmap
	 cr1 = (-b / (2 * a)) :+ (sqrt(-delta) / (2 * a))
	 cr2 = (-b / (2 * a)) :+ (-sqrt(-delta) / (2 * a))
	 rr1 = ((-b - sqrt(delta)) / (2 * a)) :+ 0
	 rr2 = ((-b + sqrt(delta)) / (2 * a)) :+ 0
	 rr0 = rr1

yankLeft :: Equation -> Equation
yankLeft (Eq (lp, rp)) = Eq (lp ++ (move rp), [zero])
  where
    move [] = []
    move ((c :*^: p):rpn) = ((-c) :*^: p):move(rpn)


canonify :: Equation -> Equation
canonify (Eq (l,r)) = let l' = zero : zeroP 1 : zeroP 2 : l
			  Eq (l'', r') = yankLeft $ Eq (l', r)
			  l''' = squeeze l''
			 in Eq (l''', r')

canonifyP :: Polynomial -> Polynomial
canonifyP pl = let pl' = zero : zeroP 1 : zeroP 2 : pl
		 in squeeze pl'


degree :: M.IntMap Float -> Int
degree mmap = highest 0
	where
	  highest k = case M.lookupGT k mmap of { Nothing -> smaller k;	Just (kn, _) -> highest kn; }
	  smaller k = case M.lookupLE k mmap of { Nothing -> k;		Just (kn, v) -> check kn v; }
	  check kn v = if ( v == 0 && kn > 0 )	then	smaller (kn - 1)
				   		else	kn


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
	handleNegativePowers ( applyRcp mx pl )

    applyRcp :: Monomial -> Polynomial -> Polynomial
    applyRcp negmn pl = canonifyP $ fmap (/^/ negmn) pl

    badRoots :: Monomial -> [ComplexF]
    badRoots (_ :*^: 0) = [] -- Should not happen.
    badRoots (c :*^: p)	| (-p) == 1 = ((:[]) . (:+ 0) . solveDeg1) $ toMap [zero, (c :*^: 1), zeroP 2]
			| (-p) == 2 = bundle $ roots $ toMap [(c :*^: 2), zeroP 1, zeroP 0]
			| otherwise = error $ "Will not solve: monomial of a higher absolute degree that 2:\n" ++ prettyMonomial (c :*^: p)

    findNeg = find (\(c :*^: p) -> p < 0 && c /= 0)


