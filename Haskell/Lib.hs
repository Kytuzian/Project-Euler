module Lib (memoize,
            n, z, znonzero,
            mapPair, flipPair, pairRatio, makePair,
            sumPattern,
            pentagonal, pentagonalNumbers, triangleNumber,
            quadratic, intQuadratic,
            nubOnSorted, mapUntil,
            combinationElements,
            divides,
            defaultIfNothing,
            intersperseBy,
            count, countDuplicates, unduplicate,
            remove, removeAll, setAt, insertReplace,
            differences, ratios, ratioTo,
            factorial,
            isPermutation) where

    import Data.List
    import Data.Ratio

    import qualified Data.Map as Map
    import Data.IORef
    import System.IO.Unsafe

    import qualified Math.NumberTheory.Primes.Factorisation as Factorisation

    mapPair :: (a -> b) -> (a, a) -> (b, b)
    mapPair f (a, b) = (f a, f b)

    flipPair :: (a, b) -> (b, a)
    flipPair (a, b) = (b, a)

    pairRatio :: Integral a => (a, a) -> Ratio a
    pairRatio (a, b) = a % b

    makePair :: [a] -> (a, a)
    makePair (a:b:_) = (a, b)

    unduplicate :: [(Int, a)] -> [a]
    unduplicate [] = []
    unduplicate ((i, x):xs) = replicate i x ++ unduplicate xs

    isPermutation :: String -> String -> Bool
    isPermutation a b = sort a == sort b

    combinationElements :: [[a]] -> [[a]]
    combinationElements (x:[]) = [[i] | i <- x]
    combinationElements (x:xs) = [i : nc | i <- x, nc <- combinationElements xs]

    constant a _ = a

    memoize f = unsafePerformIO $ do
        r <- newIORef Map.empty
        return $ \ x -> unsafePerformIO $ do
            m <- readIORef r
            -- print x
            case Map.lookup x m of
                Just y  -> return y
                Nothing -> do
                        let y = f x
                        writeIORef r (Map.insert x y m)
                        return y

    nubOnSorted (a:[]) = [a]
    nubOnSorted (a:b:vs)
        | a == b = nubOnSorted (b : vs)
        | otherwise = a : nubOnSorted (b : vs)

    insertReplace vs newV i = take i vs ++ newV ++ drop (i + 1) vs

    sumPattern ns pattern = sum (zipWith (*) ns fullPattern)
        where fullPattern = (concat . take (length ns) . repeat) pattern

    pentagonal n = n * (3 * n - 1) `div` 2

    pentagonalNumbers limit = takeWhile (<= limit) (map pentagonal znonzero)

    intersperseBy _ [] = []
    intersperseBy f (l:ls) = l ++ [f (last l)] ++ intersperseBy f ls

    n = [1..]

    z = 0 : [y | n <- [1..], y <- [n, (-n)]]

    znonzero = tail z

    divides b a = a `mod` b == 0

    mapUntil :: (a -> b) -> (b -> Bool) -> [a] -> [b]
    mapUntil f pred = takeWhile pred . map f

    defaultIfNothing _ (Just a) = a
    defaultIfNothing def Nothing = def

    halve :: [a] -> ([a], [a])
    halve [] = ([],[])
    halve xs = go xs xs
        where go (x:xs) (_:_:ys) = let (first,last) = go xs ys in (x:first, last)
              go (x:xs) [_] = ([x],xs)
              go (x:xs) []  = ([],x:xs)

    remove :: Eq a => [a] -> a -> [a]
    remove [] _ = []
    remove (x:xs) e
      | x == e = xs
      | otherwise = x : remove xs e

    removeAll :: Eq a => [a] -> a -> [a]
    removeAll xs e = filter (/= e) xs

    count :: (a -> Bool) -> [a] -> Int
    count _ [] = 0
    count f (x:xs)
        | f x = 1 + count f xs
        | otherwise = count f xs

    countDuplicates :: Eq a => [a] -> [(Int, a)]
    countDuplicates [] = []
    countDuplicates (x:xs) = (count (== x) xs + 1, x) : countDuplicates (removeAll xs x)

    differences :: Num a => [a] -> [a]
    differences (a:b:[]) = [b - a]
    differences (a:b:bs) = (b - a) : differences (b : bs)

    ratioTo :: Integral a => a -> [a] -> [Ratio a]
    ratioTo test (a:[]) = [a % test]
    ratioTo test (a:as) = (a % test) : ratioTo test as

    ratios :: Integral a => [a] -> [Ratio a]
    ratios (a:b:[]) = [b % a]
    ratios (a:b:bs) = (b % a) : ratios (b : bs)

    triangleNumber :: Integral a => a -> a
    triangleNumber n = n * (n + 1) `div` 2

    -- triangleN :: Integral a => a -> a
    triangleN n = fst $ intQuadratic 1 1 ((-2) * n)

    intQuadratic a b c = mapPair round $ quadratic a b c

    quadratic a b c = (x0, x1)
        where x0 = ((-b) + (sqrt (b^2 - 4*a*c))) / (2 * a)
              x1 = ((-b) - (sqrt (b^2 - 4*a*c)) ) / (2 * a)

    setAt :: [a] -> Int -> a -> [a]
    setAt xs i e = take i xs ++ [e] ++ drop (i + 1) xs

    factorial n = product [1..n]

    nestedRadical f = unfoldr nestedRadical' (0, 0)
        where nestedRadical' (a, b) = Just (nextVal, (nextVal, b + 1))
                where nextVal = sqrt (a + (f b))

    series f k total = nextVal : series f (k + 1) nextVal
        where nextVal = f k + total

    showApproxFunc f = mapM_ (\i -> putStrLn (show (f i))) [1..]

    showSeries f k total = do
        mapM_ (\i -> putStr ("\r" ++ show (fromRational i) ++ "                 ")) (series f k total)
