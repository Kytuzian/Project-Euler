import Lib (intSqrt)

import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli

p24Test p = (sum $ map (\x -> powerMod x (p + 1) p) [2,3,5,7]) `mod` p == 0

p20 = [(a, b, c, d, e) | n <- [1..], a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], e <- [1..n], (a + 1) * (3*b*c+1) == d + 3*e + 1, (b + 1) * (3*a*c+1) == 3*d + e + 13, (c + 1) * (3*a*b+1) == 4*(26 - d - e) - 1]

p26 = [(a, b) | a <- [1..2016^2], b <- [1..a - 1], (a - b) * sqrt (a * b) == 2016]

p28 = [(x, y) | n <- [1..], x <- [1..n], y <- [1..n], (x^2 + 1)*(y^2+1)+2*(x-y)*(1-x*y) == 4*(1+x*y) + 140]

p14 = [(p,q,r) | p <- take 400 primes, q <- take 400 primes, r <- take 400 primes, p*q + q * r + r*p == 2016]

main = do
    mapM_ print p20
