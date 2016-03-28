import Lib (pairOverlap, flatten, pascalsTriangle)

import Data.List
import Math.NumberTheory.Primes.Factorisation

isSquareFree :: Integral a => a -> Bool
isSquareFree n = all (\(_, count) -> count < 2) $ factorise $ fromIntegral n

main = do
    let ls = take 50 pascalsTriangle
    let ns = nub $ filter isSquareFree $ flatten ls
    print $ sum ns
