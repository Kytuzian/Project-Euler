import Data.Ratio
import Data.List

import qualified Math.NumberTheory.Primes.Factorisation as Factorisation

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, b) = (f a, f b)

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

pairRatio :: Integral a => (a, a) -> Ratio a
pairRatio (a, b) = a % b

unduplicate :: [(Int, a)] -> [a]
unduplicate [] = []
unduplicate ((i, x):xs) = replicate i x ++ unduplicate xs

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

isCoprime :: Int -> Int -> Bool
isCoprime a b = gcd a b == 1

totient :: Int -> Int
totient n = numerator $ (n % 1) * (foldl (*) 1 $ map (\i -> 1 - 1 % i) factorisation)
    where factorisation = nub $ map fromIntegral $ unduplicate $ map flipPair $ Factorisation.factorise (toInteger n)

isPermutation :: String -> String -> Bool
isPermutation a b = sort a == sort b

doP70 limit = map (\(a, b) -> (pairRatio (a, b), a)) permutationTotients
    where totients = zip [2..limit] (map totient [2..limit])
          permutationTotients = filter (uncurry isPermutation . pairMap show) totients

main = print $ doP70 (10^7)
