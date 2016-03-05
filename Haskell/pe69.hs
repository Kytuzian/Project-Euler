import Lib(divides, firstWhere, sieveBy, genSieve, defaultIfNothing)

import Data.List
import Data.Numbers.Primes
import Data.Ratio

--products of primes produces the various maxes of the gcfs pe69, so 2*3*5*7*11*13... gives the successive maximums

isCoprime a b = 1 == gcd a b

phi n = length $ filter (isCoprime n) [1..n]

main = print $ map phi3 [1..2000]

phi3 n = length (1 : coprime)
    where coprime = map fst $ filter snd $ map (\(i, a) -> (i, not $ defaultIfNothing False a)) notCoprime
          notCoprime = genSieve (Just . not . isCoprime n) (\(i, _) -> (i + 1, Just True)) [2..n]

phiRatio n = n % phi n

pe69 n = (snd . head . reverse . sort) $ zip (map phiRatio [30030,60060..n]) [30030,60060..]

pe69s = nub $ map pe69 [30030,60060..]

partialGCFs [] = []
partialGCFs ns = foldl gcd (maximum ns) ns : partialGCFs (tail ns)
