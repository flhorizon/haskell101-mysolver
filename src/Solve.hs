
module Solve (
	verboseSolve
	) where

import Control.Applicative ((<*>))
import qualified Data.DList as D
import Data.Complex (realPart)
import Control.Monad.Writer

import Data.MyPolynomial
import Data.MyPolynomial.Print


data Solvability = Absurd | Divergent | Degree | Clear


discriminantSpeech :: Float -> Writer (D.DList String) ()
discriminantSpeech d = do let s = case (compare d 0) of	LT -> "strictly negative, the two complex solutions are:\n"
							GT -> "strictly positive, the two real solutions are:\n"
							EQ -> "null, the unique solution is:\n"
				in tell $ D.fromList ["The discriminant is ", s]



solvability :: Equation -> Solvability
solvability eq = 
	let [a, d] = [absurd, divergent] <*> [eq]
	  in case [a, d] of	[True, _] -> Absurd
				[False, True] -> Divergent
				[False, False] -> Clear



unrootableSpeech :: Solvability -> Writer (D.DList String) ()
unrootableSpeech Absurd = tell $ D.singleton "Cannot solve: absurd.\n"
unrootableSpeech Divergent = tell $ D.singleton "The solution is |R itself.\n"
unrootableSpeech Degree = tell $ D.singleton "The polynomial degree is strictly greater than 2, I can't solve."
unrootableSpeech _ = return ()


verboseSolve :: Equation -> Writer (D.DList String) (Equation, Maybe Roots) 
verboseSolve eq = run
  where
    ceq = condense eq
    deg = degree $ getL ceq
    run = do
    	tell $ D.fromList ["Reduced form: ", (prettyEquation ceq), "\n"]
	tell $ D.fromList ["Polynomial degree: ", show deg, "\n"]
	let rootable = solvability ceq
	  in case rootable of	Clear -> solve
				_ -> quit rootable

    quit rtb = do
    	unrootableSpeech rtb
	return (ceq, Nothing)

    solve = do
    	let coeffs = getCoeffs $ getL ceq
	let delta = discriminantQuadratic coeffs
	let rts@(rt1, rt2) = roots coeffs

	discriminantSpeech delta
	tell $ D.fromList [prettyComplex rt1, "\n", prettyComplex rt2]
	return (ceq, Just rts)
