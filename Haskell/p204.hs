import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation

import Lib (showProgress, incrementDigitsIf)

import Data.List

hammingNumber :: Integral a => a -> a -> Bool
hammingNumber n = hammingNumber'
    where hammingNumber' i = (fromIntegral $ fst $ last $ factorise $ fromIntegral i) <= n

generateHammingNumbers n limit = generateHammingNumbers' $ replicate (length ps) 0
    where ps = takeWhile (<= n) primes
          generateHammingNumbers' ns@(n:_)
            | length nextNs > length ns = [current]
            | otherwise = current : generateHammingNumbers' nextNs
            where current = product $ zipWith (^) ps ns
                  nextNs = incrementDigitsIf ((<= limit) . product . zipWith (^) ps) ns

main = do
    let limit = 10^9
    let ns = generateHammingNumbers 100 limit
    mapM_ print ns
    print $ length ns
