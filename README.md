'mySolver'
=======================

A projet to learn Haskell by pratice.

A quadratic equation parser / solver.

## Build

### Dependencies
- GNU make 4
- ghc 7.6.3
- dlist 0.8.0.2

`$ cabal update` 

`$ cabal install dlist` 

`$ make` 

## Example
`$ ./mySolver 'X + 1 = -X^-1'`
```
Reduced form: 1.0 * X^0 + 1.0 * X^1 + 1.0 * X^2 = 0
Natural reduced form: 1.0 + 1.0 X + 1.0 X^2 = 0
Polynomial degree: 2
Excluded roots/solutions:
-0.0
The discriminant is strictly negative, the two complex solutions are:
-0.5 + 0.8660254i
-0.5 - 0.8660254i
```
