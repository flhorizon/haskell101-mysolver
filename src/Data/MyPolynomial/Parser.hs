
 
module Data.MyPolynomial.Parser (
	readsMonomial
	, readsPolynomial
	, readsEquation
	, readEquation
	) where


import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import qualified Data.DList as D

import Data.MyPolynomial.Type
 
 -- -- -- --
 -- -- -- --
 --
 --	Private monadic helpers.
 --
 --

toDL :: Monad m =>  [a] -> m (D.DList a)
toDL l = return $ D.fromList l

sToDL :: Monad m =>  a -> m (D.DList a)
sToDL c = return $ D.singleton c

toL :: Monad m =>  (D.DList a) -> m [a]
toL dl = return $ D.toList dl



 -- -- -- --
 -- -- -- --
 --
 --	Parsers.
 --
 --


floatLitteral :: ReadP String
floatLitteral = do
	sm <- option "" ( string "-" )  >>= toDL
	intdgs <- many1 ( satisfy isDigit ) >>= toDL	-- 1 or more digits before the .
	dec <- decimalPart <++ return D.empty		-- maybe dot digits
	exp <- exponentPart <++ return D.empty		-- maybe E (maybe '-') digits
	let float = (D.toList . D.concat) [sm, intdgs, dec, exp]
	  in return float
	  where
	    decimalPart = do
	    	c <- char '.' >>= sToDL
	    	dgs <- many1 ( satisfy isDigit ) >>= toDL
		return $ c `D.append` dgs
	    exponentPart = do
	    	e <- char 'e' +++ char 'E'  >>= sToDL
		m <- option "" ( string "-" ) >>= toDL
	    	dgs <- many1 ( satisfy isDigit ) >>= toDL
		return $ D.concat [e, m, dgs]


 -- Parse monomial
parseMnl :: ReadP Monomial
parseMnl = do
	skipSpaces
	mc <- minusOrVoid
	skipSpaces
	lTok <- floatLitteral		-- Coefficient
	pow <- option 0 consPower	-- implicit 0 power if missing the cons part.
	return $ (read $ mc lTok :: Float) :*^: pow
	  where
	  -- -- -- --
	  -- Helpers
	  -- -- -- --
	    consPower :: ReadP Int
	    consPower = do
		skipSpaces			-- -- -- -- -- --
		char '*'			--
		skipSpaces			--
		satisfy (\c -> c `elem` "xX")	-- Monomial << Constructor >>
		skipSpaces			--  * x ^
		char '^'			--
		skipSpaces			-- -- -- -- -- --
		mp <- minusOrVoid		-- Power ... 
		rTok <- many1 ( satisfy isDigit ) 
		return ( read $ mp rTok :: Int )
	    -- -- -- -- -- -- -- -- --
	    -- -- -- -- -- -- -- -- --
	    minusOrVoid :: ReadP ShowS
	    minusOrVoid = do
		c <- option '#' (char '-')
		if c == '-'
			then return $ (c:)
			else return $ ("" ++)



 -- Parse polynomial; i.e. : [Monomial]
parsePnml :: ReadP (D.DList Monomial)
parsePnml  = do
	let next = do
		hp <- parseMnl
		skipSpaces
		optional $ char '+'
		skipSpaces
		parsePnml >>= ( \dl -> return ( hp `D.cons` dl ) )
	  in next <++ ( parseMnl >>= ( \hp -> return hp >>= sToDL ) )


parseEq :: ReadP Equation
parseEq = do
	skipSpaces
	hpll <- parsePnml >>= toL
	skipSpaces
	string "="
	skipSpaces
	hplr <- parsePnml >>= toL
	skipSpaces
	return $ Eq (hpll, hplr)



 -- -- -- --
 -- -- -- --
 --
 --	Public wrappers.
 --
 --


readsMonomial :: ReadS Monomial
readsMonomial = readP_to_S parseMnl


readsPolynomial :: ReadS Polynomial
readsPolynomial = readP_to_S (parsePnml >>= toL)


readsEquation :: ReadS Equation
readsEquation = readP_to_S parseEq

readEquation :: String -> Equation
readEquation s = let parsed = filter (\(_, r) -> null r) $ readsEquation s
		    in case parsed of	[] -> error "Failed to parse the equation."
					_ -> fst . last $ parsed
