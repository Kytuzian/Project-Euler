import Lib (count, zipTo, intSqrt, divides)

import Math.NumberTheory.Primes.Factorisation (divisors)
import Data.Set (size)

import System.ProgressBar

isP179 :: Integer -> Bool
isP179 n = (size $ divisors n) == (size $ divisors $ n + 1)

main = do
    let limit = 10^7
    let res = filter isP179 [1..limit]
    mapM_ (\i -> progressBar (msg $ show i ++ " of " ++ show limit) percentage 80 i limit) res
    print $ length res
