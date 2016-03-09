module Lib (memoize,
            fibMemo,
            n, z, znonzero,
            sumPattern,
            constructAggregates,
            insertReplace,
            firstWhere,
            pentagonal,
            pentagonalNumbers,
            nubOnSorted,
            divides,
            defaultIfNothing,
            splitEvery,
            intersperseBy,
            sieveBy, sieve,
            genSieve,
            count,
            removeAll,
            countElements,
            differences,
            factorial) where

    import Data.List
    import Data.Ratio

    import qualified Data.Map as Map
    import Data.IORef
    import System.IO.Unsafe

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

    buildAggregates f [] = []
    buildAggregates f vs = vs : buildAggregates f (concat (map f vs))

    constructAggregates f vs = nub (map sort aggs)
        where aggs = concat (buildAggregates f vs)

    insertReplace vs newV i = take i vs ++ newV ++ drop (i + 1) vs

    sumPattern ns pattern = sum (zipWith (*) ns fullPattern)
        where fullPattern = (concat . take (length ns) . repeat) pattern

    pentagonal n = n * (3 * n - 1) `div` 2

    pentagonalNumbers limit = takeWhile (<= limit) (map pentagonal znonzero)

    splitEvery _ [] = []
    splitEvery n l = take n l : splitEvery n (drop (n + 1) l)

    intersperseBy _ [] = []
    intersperseBy f (l:ls) = l ++ [f (last l)] ++ intersperseBy f ls

    sieve n = sieveBy (constant (Just True)) (\(i, _) -> (i + 1, Just False)) [2..n]

    genSieve f gen ns = genSieve' (zip ns (replicate (length ns) Nothing))
        where genSieve' [] = []
              genSieve' ((i, Nothing):xs) = case f i of
                                             Just True -> (i, Just True) : genSieve' (intersperseBy gen (splitEvery (i - 1) xs))
                                             Just False -> (i, Just False) : genSieve' xs
              genSieve' ((i, v):xs) = (i, v) : genSieve' xs

    sieveBy f gen ns = map fst $ filter (defaultIfNothing False . snd) $ genSieve f gen ns

    n = [1..]

    z = 0 : [y | n <- [1..], y <- [n, (-n)]]

    znonzero = tail z

    fib 0 = 1
    fib 1 = 1
    fib n = fibMemo (n - 1) + fibMemo (n - 2)
    fibMemo = memoize fib

    firstWhere f ls = firstWhere' (dropWhile (not . f) ls)
        where firstWhere' [] = Nothing
              firstWhere' ls = Just (head ls)

    divides b a = a `mod` b == 0

    defaultIfNothing _ (Just a) = a
    defaultIfNothing def Nothing = def

    count a = length . filter (a ==)

    removeAll e = filter (/= e)

    countElements [] = []
    countElements (n:ns) = count n ns : countElements (removeAll n ns)

    differences :: Num a => [a] -> [a]
    differences (a:b:[]) = [b - a]
    differences (a:b:bs) = (b - a) : differences (b : bs)

    factorial n = product [1..n]

    nestedRadical f = unfoldr nestedRadical' (0, 0)
        where nestedRadical' (a, b) = Just (nextVal, (nextVal, b + 1))
                where nextVal = sqrt (a + (f b))

    series f k total = nextVal : series f (k + 1) nextVal
        where nextVal = f k + total

    showApproxFunc f = mapM (\i -> putStrLn (show (f i))) [1..]

    showSeries f k total = do
        mapM (\i -> putStr ("\r" ++ show (fromRational i) ++ "                 ")) (series f k total)
