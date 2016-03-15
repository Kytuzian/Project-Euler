import Data.Ratio
import Data.List

import Lib (flatten, nubOnSorted)

fracs limit = map (% limit) [1..limit]

reducedProperFractions d limit = map (nub . reducedProperFractions') [1..d]
    where reducedProperFractions' i
            | i <= d = filter ((== i) . denominator) $ filter (< limit) [1 % i, 2 % i..limit + 1]
            | otherwise = []

p73 lower upper limit = map (filter (> lower)) $ reducedProperFractions limit upper

solvedP73 = p73 (1 % 3) (1 % 2) 12000

main = do
    let res = map length solvedP73
    mapM_ print $ zip [0..] $ res
    print $ sum $ res
