import Lib (sumProperDivisors, zipTo, showProgressZipped)

import Data.List

import System.ProgressBar

isAmicable :: Integral a => a -> Bool
isAmicable n = sumProperDivisors n > n

amicableChain :: Integral a => a -> a -> [a]
amicableChain limit n = reverse $ amicableChain' [n] n
    where amicableChain' xs i
            | last xs == nextN = xs
            | nextN `elem` xs = []
            | i == 1 || nextN == 1 = []
            | nextN > limit = []
            | otherwise = amicableChain' (nextN : xs) nextN
            where nextN = sumProperDivisors i

main = do
    let limit = 10^6
    let chains = zipTo (amicableChain limit) $ filter isAmicable [1..limit]
    let applicableChains = filter (\(_,v) -> all (< limit) v) $ filter ((> 0) . length . snd) chains
    let longestChain = maximumBy (\(_,a) (_, b) -> (length a) `compare` (length b)) applicableChains
    showProgressZipped limit chains
    mapM_ print applicableChains
    print longestChain
    print $ minimum $ snd longestChain
