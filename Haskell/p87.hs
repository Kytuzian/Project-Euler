import qualified Math.NumberTheory.Primes as Primes

import Data.List

import Lib (memoize)

sumPrimePower :: [Int] -> Int
sumPrimePower ns = sum $ zipWith (^) ns [2..]

incrementEach :: [Int] -> [[Int]]
incrementEach [] = []
incrementEach (x:xs) = [x + 1: xs] ++ (map (x :) $ incrementEach xs)

incrementDigitsIf :: ([Int] -> Bool) -> [Int] -> [Int]
incrementDigitsIf _ [] = [1]
incrementDigitsIf f num@(d:ds)
    | f num = d + 1 : ds
    | otherwise = 0 : incrementDigitsIf f ds

nthPrime n = fromIntegral $ Primes.primes !! n

-- sumPrimePowers :: Int -> [Int] -> [Int]
sumPrimePowers limit ns
    | length next > 0 = nextRes ++ (foldl (++) [] $ map (memSumPrimePowers limit) $ nextNs)
    | otherwise = []
    where next = filter (\i -> fst i < limit) $ map (\i -> (sumPrimePower (map nthPrime i), i)) $ incrementEach ns
          nextNs = map snd next
          nextRes = map fst next
          ps = map nthPrime ns

memSumPrimePowers = memoize sumPrimePowers

p87 limit = nub $ 28 : sumPrimePowers limit [0,0,0]

main = print $ length $ p87 2500
