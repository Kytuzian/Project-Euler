import Lib (incrementDigitsToIf, incrementDigitsIf, zipTo, unduplicate, countDuplicates,
            unPair, combinations, showProgressZipped)

import Data.List
import Data.Ord

import Math.NumberTheory.Primes

import System.IO

replaceInfix :: Eq a => [a] -> [a] -> [a] -> [a]
replaceInfix _ _ [] = []
replaceInfix cs es xs@(x:nextXs)
    | take (length cs) xs == cs = es ++ drop (length cs) xs
    | otherwise = x : replaceInfix cs es nextXs

allCombinations :: Eq a => [a] -> [[a]]
allCombinations ns = concat $ map (\i -> combinations i ns) [1..length ns]

isProductSum :: Integral a => [a] -> Bool
isProductSum ns = sum ns == product ns

factor :: Integral a => a -> [a]
factor n = factor' n 2
    where factor' 1 _ = []
          factor' n i
            | n `mod` i == 0 = i : factor' (n `div` i) 2
            | otherwise = factor' n (i + 1)

-- isPrime :: Integral a => a -> Bool
-- isPrime n = (length $ factor n) == 1

divisorPairs :: Integral a => a -> [[a]]
divisorPairs n = [[d, n `div` d] | d <- [1..n `div` 2], n `mod` d == 0]

allProds :: Integral a => a -> [[a]]
allProds n = [n] : (nub $ map sort $ concat $ map allProds' $ divisorPairs n)
    where allProds' inNs = case ns of
                            [] -> []
                            _ -> res ++ (concat $ map allProds' res)
            where ns = filter (/= 1) inNs
                  res = concat $ map (\(x, xs) -> map (\cs -> replaceInfix [x] cs ns) xs) $ zipTo (tail . divisorPairs) $ filter (not . isPrime . fromIntegral) ns

canBeProductSumOf :: Int -> [Int] -> Bool
canBeProductSumOf n ns = length ns + product ns - sum ns == n

minProdSumsOf :: Int -> [[Int]]
minProdSumsOf n = map (\xs -> replicate (product xs - sum xs) 1 ++ xs) $ allProds n

minProdSum :: Int -> [Int]
minProdSum n = minProdSum' 1
    where minProdSum' i = case find (canBeProductSumOf n) $ allProds i of
                            Nothing -> minProdSum' (i + 1)
                            Just xs -> replicate (product xs - sum xs) 1 ++ xs

new :: Int -> IO [(Int, Int)]
new limit = do
    let res = nubBy (\a b -> length a == length b) $ concat $ map minProdSumsOf [2..limit]

    let sums = tail $ map (\ns -> (length ns, sum ns)) res

    return $ nubBy (\(_, a) (_, b) -> a == b) sums

cur :: Int -> IO [(Int, Int)]
cur limit = do
    -- putStrLn $ "Generating minimum product sums from 2 to " ++ show limit ++ "."
    let res = map minProdSum [2..limit]
    -- showProgressZipped (fromIntegral limit) $ zip [2..] res
    -- putStrLn ""

    let sums = zip [2..] $ map sum res

    return $ nubBy (\(_, a) (_, b) -> a == b) sums

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    -- curRes <- cur 200
    -- oldRes <- old 150
    newRes <- new 400

    print newRes
    print $ sort $ map snd newRes
