
module MyPolynome.Print
 where

import Data.Complex
import MyPolynome

prettyMonomeS :: Monome -> ShowS
prettyMonomeS (Monome c p) = prettyMonomeS (c :*^: p)
prettyMonomeS (c :*^: p) = shows c . (" * X^" ++) . shows p

prettyPolynomeS :: Polynome -> ShowS
prettyPolynomeS []  = ("{Empty set}" ++)
prettyPolynomeS (mn:[]) = prettyMonomeS mn
prettyPolynomeS (m1:(c :*^: p):ms) = prettyPolynomeS (m1:(Monome c p):ms)
prettyPolynomeS (m1:(m2@(Monome c2 p2)):ms) = prettyMonomeS m1 . signBridge . next
 where
  signBridge
   | c2 < 0 = (" - " ++)
   | otherwise = (" + " ++)
  next
   | c2 < 0 = prettyPolynomeS ((Monome (-c2) p2):ms)
   | otherwise = prettyPolynomeS (m2:ms)

prettyMonome :: Monome -> String
prettyMonome m = prettyMonomeS m []

prettyPolynome :: Polynome -> String
prettyPolynome p = prettyPolynomeS p []
