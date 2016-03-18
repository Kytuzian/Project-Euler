import qualified Data.Set as Set

import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation

import System.ProgressBar

import Lib

intSqrt :: Integral a => a -> a
intSqrt = round . sqrt . fromIntegral

p357 n = all (\d -> isPrime $ d + n `div` d) divs
    where divs = Set.toList $ divisors n
          intSqrtN = intSqrt n

main = do
    let res = 1 : filter p357 [2,4..100000000]
    mapM_ (\i -> progressBar (msg "Working") percentage 80 i 100000000) res
    print $ sum res
