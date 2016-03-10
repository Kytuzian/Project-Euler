import Data.Ratio
import Data.List

--Doesn't really solve the problem, but I used it to find the pattern (numerators increase by 3, denominators increase by 7, simple arithmetic sequence).

-- reducedProperFractions :: Int -> Ratio Int -> [Ratio Int]
reducedProperFractions d limit = nub $ reducedProperFractions' 2
    where reducedProperFractions' i
            | i <= d = filter (< limit) [1 % i, 2 % i..limit + 1] ++ reducedProperFractions' (i + 1)
            | otherwise = []

doP71s limit = mapM_ print $ nubBy (\a b -> snd a == snd b) $ map (\i -> (i, last $ sort $ reducedProperFractions i (3 % 7))) [3..limit]

main = doP71s 1000
