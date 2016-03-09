import qualified Math.NumberTheory.Primes as Primes

import Data.List

import Lib (memoize)

sumPrimePower :: [Int] -> Int
sumPrimePower ns = sum $ zipWith (^) ns [2..]

incrementEach :: [Int] -> [[Int]]
incrementEach [] = []
incrementEach (x:xs) = [x + 1: xs] ++ (map (x :) $ incrementEach xs)

incrementAt :: [Int] -> Int -> [Int]
incrementAt xs i = take i xs ++ [e + 1] ++ drop (i + 1) xs
    where e = xs !! i

setAt :: [Int] -> Int -> Int -> [Int]
setAt xs i e = take i xs ++ [e] ++ drop (i + 1) xs

incrementDigitsIf :: ([Int] -> Bool) -> [Int] -> Int -> [Int]
incrementDigitsIf f ds i
    | i >= length ds = [0]
    | f (incrementAt ds i) = incrementAt ds i
    | otherwise = incrementDigitsIf f (setAt ds i 0) (i + 1)

nthPrime n = fromIntegral $ Primes.primes !! n

-- sumPrimePowers :: Int -> [Int] -> [Int](sumPrimePower $ map nthPrime i) < 1000
sumPrimePowers limit ns
    | length nextNs == length ns = sumPrimePower ps : sumPrimePowers limit nextNs
    | otherwise = []
    where ps = map nthPrime ns
          nextNs = incrementDigitsIf ((< limit) . sumPrimePower . map nthPrime) ns 0

p87 limit = nub $ 28 : sumPrimePowers limit [0,0,0]

main = print $ length $ p87 (50 * 10^6)
