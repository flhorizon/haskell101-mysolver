
module Data.MyPolynomial.Type
	where

infixl 6 :*^:
data Monomial = Float :*^: Int
  deriving (Eq)

type Polynomial = [Monomial] 
newtype Equation = Eq { getEq :: (Polynomial, Polynomial) }

getL :: Equation -> Polynomial
getL (Eq (l, _)) = l

getR :: Equation -> Polynomial
getR (Eq (_, r)) = r

mbrCoeff :: Monomial -> Float
mbrCoeff ( c :*^: _ ) = c

mbrPower :: Monomial -> Int
mbrPower ( _ :*^: p ) = p

 -- Smart constructor enforcing power sign constraint.
infixl 6 *^
(*^) :: Float -> Int -> Monomial
c *^ p	| p >= 0 = c :*^: p
	| otherwise = error "Constructing a monomial with a negative power."

zeroP :: Int -> Monomial
zeroP pw = 0 :*^: pw

zero :: Monomial
zero = zeroP 0

instance Ord Monomial where
  m1@(c1 :*^: p1) `compare` m2@(c2 :*^: p2)
    | m1 == m2 = EQ
    | (p1 < p2) || ((p1 == p2) && (c1 < c2)) = LT
    | (p1 > p2) || ((p1 == p2) && (c1 > c2)) = GT
