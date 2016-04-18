import Lib (zipTo, flipPair)

import Math.NumberTheory.Primes.Factorisation

import Data.List
import Data.Ord

rad = product . map fst . factorise

main = do
    let limit = 10^5
    let unsorted = zipTo rad [1..limit]
    let sorted = sortBy (comparing flipPair) unsorted
    let final = zipWith (\k (a, b) -> (a, b, k)) [1..] sorted
    mapM_ print final
    print $ final !! (10^4 - 1)
