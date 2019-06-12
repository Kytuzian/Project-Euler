import Lib (incrementDigitsToIf, incrementDigitsIf, zipTo, unduplicate, countDuplicates,
            unPair, combinations, showProgressZipped)

import Data.List
import Data.Ord

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

isPrime :: Integral a => a -> Bool
isPrime n = (length $ factor n) == 1

divisorPairs :: Integral a => a -> [[a]]
divisorPairs n = [[d, n `div` d] | d <- [1..n `div` 2], n `mod` d == 0]

-- allProds :: Integral a => a -> [[[a]]]
allProds n = [n] : (nub $ map sort $ concat $ map allProds' $ divisorPairs n)
    where allProds' inNs = case ns of
                            [] -> []
                            _ -> res ++ (concat $ map allProds' res)
            where ns = filter (/= 1) inNs
                  res = concat $ map (\(x, xs) -> map (\cs -> replaceInfix [x] cs ns) xs) $ zipTo (tail . divisorPairs) $ filter (not . isPrime) ns

replaceProduct ns = map (\(xs, cs) -> map (\c -> replaceInfix c [product c] xs) cs) ns

allProducts :: Integral a => a -> [[a]]
allProducts n = nub $ concat $ replaceProduct $ zipTo allCombinations divisors
    where divisors = nub $ allCombinations $ factor n

-- canBeProductSumOf :: Integral a => Int -> [Int] -> Bool
canBeProductSumOf n ns = length ns + product ns - sum ns == n

-- minProdSumsOf :: Integral a => a -> [[a]]
minProdSumsOf n = map (\xs -> replicate (product xs - sum xs) 1 ++ xs) $ allProds n

-- minProdSum :: Integral a => a -> [a]
minProdSum n = minProdSum' 1
    where minProdSum' i = case find (canBeProductSumOf n) $ allProds i of
                            Nothing -> minProdSum' (i + 1)
                            Just xs -> replicate (product xs - sum xs) 1 ++ xs

-- minProductSum :: Integral a => a -> [a]
minProductSum n = minProductSum' [n]
    where minProductSum' ns
            | isProductSum res = res
            | otherwise = minProductSum' (nextHead : nextTail)
            where nextTail = incrementDigitsIf ((< n) . sum) $ tail ns
                  nextHead = n - sum nextTail
                  res = unduplicate $ zip ns [1..]

old limit = do
    -- First, create all the min product sums of the numbers from 2 to the limit
    -- Then, we only want to keep the smallest one of each length
    -- putStrLn $ "Generating minimum product sums from 2 to " ++ show limit ++ "."
    let generatedMinProdSums = map minProdSumsOf [2..limit]
    -- showProgressZipped (fromIntegral limit) $ zip [2..] $ map length generatedMinProdSums
    -- putStrLn ""

    -- putStrLn "Sorting generated mininum product sums."
    let sortedGeneratedRes = sortBy (comparing length) $ concat generatedMinProdSums
    -- putStrLn "Grouping and finalizing the result."
    let finalGeneratedRes = map head $ groupBy (\a b -> length a == length b) $ sortedGeneratedRes
    -- showProgressZipped (fromIntegral limit) $ zip [2..] $ map length finalGeneratedRes
    -- putStrLn ""

    -- We're going to be missing some lengths, so let's find which ones
    -- Then generate those lengths using minProdSum

    let missingLengths = [2..limit] \\ (map length finalGeneratedRes)
    let missingRes = map minProdSum missingLengths

    -- showProgressZipped (fromIntegral $ length missingLengths) $ zip [1..fromIntegral $ length missingLengths] $ map length missingRes
    -- putStrLn ""

    -- Combine both lists
    let finalRes = finalGeneratedRes ++ missingRes

    let sums = zip [2..] $ tail $ map sum finalRes

    return $ nubBy (\(_, a) (_, b) -> a == b) sums

new limit = do
    let res = nubBy (\a b -> length a == length b) $ concat $ map minProdSumsOf [2..limit]

    let sums = zip [2..] $ tail $ map sum res

    return $ nubBy (\(_, a) (_, b) -> a == b) sums

cur limit = do
    -- putStrLn $ "Generating minimum product sums from 2 to " ++ show limit ++ "."
    let res = map minProdSum [2..limit]
    -- showProgressZipped (fromIntegral limit) $ zip [2..] res
    -- putStrLn ""

    let sums = zip [2..] $ map sum res

    return $ nubBy (\(_, a) (_, b) -> a == b) sums

main = do
    hSetBuffering stdout NoBuffering

    -- curRes <- cur 200
    -- oldRes <- old 150
    newRes <- new 12000

    -- print curRes
    -- print $ sum $ map snd curRes
    -- print oldRes
    print newRes
    print $ sort $ map snd newRes

    -- mapM_ print $ zip (sort curRes) (sort oldRes)
