
module Solve (
	verboseSolve
	) where

import qualified Data.DList as D
import qualified Data.IntMap.Lazy as M
import Data.Complex (realPart, Complex((:+)))
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

import Data.MyPolynomial
import Data.MyPolynomial.Print

type ComplexF = Complex Float
type ShowD = D.DList Char
data Solvability b = IsBroad b | Degree | Clear





sToShowD :: String -> D.DList ShowD
sToShowD s = (D.singleton . D.fromList) s

toShowD :: [String] -> D.DList ShowD
toShowD ls = ( D.fromList . ( fmap D.fromList ) ) ls





discriminantSpeech :: Float -> Writer (D.DList ShowD) ()
discriminantSpeech d = let s = case (compare d 0) of	LT -> "strictly negative, the two complex solutions are:\n"
							GT -> "strictly positive, the two real solutions are:\n"
							EQ -> "null, the unique solution is:\n"
				in tell $ toShowD ["The discriminant is ", s]




unrootableSpeech :: Solvability BroadSol -> Writer (D.DList ShowD) ()
unrootableSpeech (IsBroad Absurd) = tell $ sToShowD "Cannot solve: absurd.\n"
unrootableSpeech (IsBroad Real) = tell $ sToShowD "The solution is |R itself.\n"
unrootableSpeech Degree = tell $ sToShowD "The polynomial degree is strictly greater than 2, I can't solve."
unrootableSpeech _ = return ()




degSolvability :: Int -> Solvability b
degSolvability d | d >= 0 && d <= 2	= Clear
		 | otherwise		= Degree


 -- Tells solution/root(s) with mention of exclusion.
tellSolutions :: Maybe Float -> Roots -> [ComplexF] -> Writer (D.DList ShowD) ()
tellSolutions delta (c1, c2) badRoots
	| delta == Just 0 = tell $ toShowD $ tellBad [c1] badRoots "root" []
	| Nothing <- delta = tell $ toShowD $ tellBad [c1] badRoots "solution" []
	| otherwise = tell $ toShowD $ tellBad [c1, c2] badRoots "root" []
  where
    tellBad (rt:rts) bad w =
    	case rt `elem` bad of	True -> ([prettyComplex rt, " (excluded " ++ w ++ ")"] ++) . next
    				_ -> ([prettyComplex rt] ++) . next
    	  where
    	    next = case rts of	(r:_) -> (["\n"] ++) . tellBad rts bad  w
    	    			[] -> ([] ++)



tellReduced :: Int -> M.IntMap Float -> Writer (D.DList ShowD) ()
tellReduced deg map = tell $ toShowD ["Reduced form: ", porcelainPolynomialSM dMap "", " = 0\n"]
	where dMap = M.filterWithKey (\k _ -> k <= deg) map

tellNatural :: M.IntMap Float -> Writer (D.DList ShowD) ()
tellNatural map = tell $ toShowD ["Natural reduced form: ", prettyPolynomialM map, " = 0\n"]


tellForbidden :: [ComplexF] -> Writer (D.DList ShowD) ()
tellForbidden [] = return ()
tellForbidden frts = tell $ toShowD $ ["Excluded roots/solutions:\n"] ++ (showForbidden frts [])
  where
    showForbidden [] = ([] ++)
    showForbidden (fr:fs) = ([prettyComplex fr, "\n"]++) . showForbidden fs





verboseSolve :: Equation -> Writer (D.DList ShowD) (Equation, Maybe Solution)
verboseSolve eq = do
	tellReduced deg mapPol
	tellNatural mapPol
	tell $ toShowD ["Polynomial degree: ", show deg, "\n"]
	tellForbidden badRoots
	case degSolvability deg of	Clear -> doSolve
					Degree -> quit Degree
  where
    Eq (cl, cr) = canonify eq
    (straightP, badRoots) = runState ( handleNegativePowers cl ) []
    ceq = Eq (straightP, cr)	-- canonified equation with negative powers lifted
    mapPol = toMap straightP	-- left polynomial as an IntMap
    deg = degree mapPol		-- left polynomial degree

    -- doSolve :: Writer (D.DList ShowD) (Equation, Maybe Solution)
    doSolve = do
    	let sol = solveEquation ceq
	  in case sol of	Quadratic _	-> result sol
				Simple _	-> result sol
				Broad b		-> quit (IsBroad b)

    -- quit :: Solvability b -> Writer (D.DList ShowD) (Equation, Maybe Solution)
    quit sb = do
    	unrootableSpeech sb
	let ret = case sb of	Degree -> Nothing
				IsBroad b -> Just (Broad b)
	return (ceq, ret)

    -- result :: Solution s -> Writer (D.DList ShowD) (Equation, Maybe Solution)
    result sol@(Quadratic rts) = do
	let delta = discriminantQuadratic mapPol
	discriminantSpeech delta
	tellSolutions (Just delta) rts badRoots
	return ( ceq, Just sol )
    result sol@(Simple fl) = do
	tell $ toShowD [ "The solution is:\n" ]
	tellSolutions Nothing (fl :+ 0, 0 :+ 0) badRoots
	return ( ceq, Just sol )


