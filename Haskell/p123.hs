import Lib (zipTo)

import Math.NumberTheory.Primes

import Data.List

primeSquareRemainder n = ((pn - 1)^n + (pn + 1)^n) `mod` pn^2
    where pn = primes !! (n - 1)

main = do
    let goal = 10^10
    let psrs = zipTo primeSquareRemainder [1,3..]
    print $ find ((> goal) . snd) psrs
