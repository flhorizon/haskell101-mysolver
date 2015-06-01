
 
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
 --	Helper parsers.
 --
 --


floatLitteral :: ReadP String
floatLitteral = do
	skipSpaces
	sm <- option "" ( string "-" )  >>= toDL
	skipSpaces
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



minusOrVoid :: ReadP ShowS
minusOrVoid = do
	c <- option '#' (char '-')
	case c of	'-' -> return (c:)
			_ -> return ("" ++)
	
		
parseCoeff :: ReadP Float
parseCoeff = do			
	lTok <- floatLitteral 
	return ( read $ lTok :: Float )

parsePower :: ReadP Int
parsePower = do
	char '^' >> skipSpaces	
	mp <- minusOrVoid	
	rTok <- skipSpaces >> many1 ( satisfy isDigit ) 
	return ( read $ mp rTok :: Int )
	
parseEqParam :: ReadP ()
parseEqParam = do
	char 'X'
	return ()


-- ( parseFloat ; option 0 ( *?; parseX) ; option 1 parsePower ))
explicitCoeffPath :: ReadP Monomial
explicitCoeffPath = do
	cf <- parseCoeff
	skipSpaces
	pow <- option 0 ( optional ( char '*' >> skipSpaces )
		>> parseEqParam >> option 1 parsePower )
	return ( cf :*^: pow )
	

-- (!parseFloat; !*; parseX ; option 1 parserPower)
implicitCoeffPath :: ReadP Monomial
implicitCoeffPath = do
	sign <- skipSpaces >> option '#' ( char '-' )
	skipSpaces
	parseEqParam
	skipSpaces
	pow <- option 1 parsePower
	case sign of	'-' -> return ( (-1.0) :*^: pow )
			_ -> return ( 1.0 :*^: pow )


 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 -- -- -- --
 -- --
 --	Parsers.
 --	
 --
 --
	

parseMonomial :: ReadP Monomial
parseMonomial = skipSpaces >> (explicitCoeffPath <++ implicitCoeffPath)

 -- Wrapper using a boolean hint about the sign being reversed.
htParseMonomial :: Bool -> ReadP Monomial
htParseMonomial False = parseMonomial
htParseMonomial _ =  parseMonomial >>= \(c :*^: p) -> return ((-c) :*^: p)


 -- Parse polynomial; i.e. : [Monomial]
parsePolynomial :: ReadP (D.DList Monomial)
parsePolynomial  = goPoly False
	where goPoly revHint = do
		let next = do
			hp <- htParseMonomial revHint
			skipSpaces
			bMinus <- look >>= \s -> case s of	('+':_) -> get >> return False
								('-':_) -> get >> return True
								_ -> pfail
			skipSpaces
			( goPoly bMinus ) >>= ( \dl -> return ( hp `D.cons` dl ) )
				in next +++ ( htParseMonomial revHint >>= \mn -> return mn >>= sToDL )


parseEq :: ReadP Equation
parseEq = do
	skipSpaces
	hpll <- parsePolynomial >>= toL
	skipSpaces
	string "="
	skipSpaces
	hplr <- parsePolynomial >>= toL
	skipSpaces
	return $ Eq (hpll, hplr)



 -- -- -- --
 -- -- -- --
 --
 --	Public wrappers.
 --
 --


readsMonomial :: ReadS Monomial
readsMonomial = readP_to_S parseMonomial


readsPolynomial :: ReadS Polynomial
readsPolynomial = readP_to_S (parsePolynomial >>= toL)


readsEquation :: ReadS Equation
readsEquation = readP_to_S parseEq

readEquation :: String -> Equation
readEquation s = let parsed = filter (\(_, r) -> null r) $ readsEquation s
		    in case parsed of	[] -> error "Failed to parse the equation."
					_ -> ( fst . last ) parsed

instance Read Equation where
  readsPrec _ str = let parsed = filter (\(_, r) -> null r) $ readsEquation str
			in case parsed of	[] -> error "Failed to parse the equation."
						_ -> [ last parsed ]

instance Read Monomial where
  readsPrec _ str = readsMonomial str
  readList str = readsPolynomial str
