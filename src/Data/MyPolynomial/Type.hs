
module Data.MyPolynomial.Type
	where


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
(*^) :: Float -> Int -> Monomial
c *^ p	| p >= 0 = c :*^: p
	| otherwise = error "Constructing a monomial with a negative power."


instance Ord Monomial where
  m1@(c1 :*^: p1) `compare` m2@(c2 :*^: p2)
    | m1 == m2 = EQ
    | (p1 < p2) || ((p1 == p2) && (c1 < c2)) = LT
    | (p1 > p2) || ((p1 == p2) && (c1 > c2)) = GT
