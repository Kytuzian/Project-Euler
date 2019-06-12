import Data.List
import Data.Ord

import Lib (count)
import MathLib (sieve, factor, toDigits, noBuffer)

import Math.NumberTheory.Primes.Sieve

-- nDigitPrimes :: Integral a => Int -> [a]
nDigitPrimes n = takeWhile (< (10^n)) $ sieveFrom (10^(n-1))

stats n = map stats' [0..9]
    where ps = nDigitPrimes n
          stats' d = [n, d, m, fromIntegral $ length matchPs, sum matchPs]
              where m = maximum $ map (count (== d) . toDigits) ps
                    matchPs = filter ((== m) . count (== d) . toDigits) ps

isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `rem` d == 0 = False
      | otherwise      = go (d+1)

-- primes = filter isPrime [2 .. ]

main = do
    noBuffer

    mapM_ (mapM_ print) $ map stats [2..10]

    -- let res = stats 10
    -- mapM_ print res
    -- print $ sum $ map (!! 3) res
    -- print $ sum $ sieve (10^6)
