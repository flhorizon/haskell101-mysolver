
module Solve (
	verboseSolve
	) where

import Control.Applicative ((<*>))
import qualified Data.DList as D
import Data.Complex (realPart)
import Control.Monad.Writer

import Data.MyPolynomial
import Data.MyPolynomial.Print

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

tellRoots :: Float -> Roots -> Writer (D.DList ShowD) ()
tellRoots delta (c1, c2)
	| delta == 0 = tell $ sToShowD ( prettyComplex c1 )
	| otherwise = tell $ toShowD [prettyComplex c1, "\n", prettyComplex c2]



verboseSolve :: Equation -> Writer (D.DList ShowD) (Equation, Maybe Solution) 
verboseSolve eq = do
    	tell $ toShowD ["Reduced form: ", (prettyEquation ceq), "\n"]
	tell $ toShowD ["Polynomial degree: ", show deg, "\n"]
	case degSolvability deg of	Clear -> doSolve
					Degree -> quit Degree
  where
    ceq = canonify eq
    mapPol = (toMap . getL) ceq
    deg = degree mapPol

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
	tellRoots delta rts
	return ( ceq, Just sol )
    result sol@(Simple fl) = do
	tell $ toShowD [ "The solution is:\n", show fl ]
	return ( ceq, Just sol )


