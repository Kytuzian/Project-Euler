module Lib (memoize,
            n, z, znonzero,
            mapPair, flipPair, pairRatio, makePair, makePairs, testPair, sumPairs,
            third,
            sumPattern, sumDigits, digits,
            pentagonal, pentagonalNumbers, triangleNumber,
            quadratic, intQuadratic,
            nubOnSorted, mapWhile, zipTo, takeUntil,
            combinationElements, sequences,
            divides,
            defaultIfNothing,
            intersperseBy,
            count, countDuplicates, countDuplicatesBy, unduplicate,
            remove, removeAll, setAt, insertReplace, insertAll, insertAllBy,
            flatten, separateList,
            differences, ratios, ratioTo, evalRatio,
            factorial,
            isPermutation,
            crossProduct2D, dotProduct,
            makeTriangle, sameSide, pointIsInTriangle,
            binarySearch,
            rollDice,
            padL, padR,
            ContinuedFraction, cfFromList, cycleCF, evalCF, period, approximateSquareRoot, makeCF) where

    import Data.List
    import Data.Ratio

    import qualified Data.Map as Map
    import Data.IORef

    import System.IO.Unsafe
    import System.Random

    import qualified Math.NumberTheory.Primes.Factorisation as Factorisation

    third :: (a, b, c) -> c
    third (_, _, c) = c

    data ContinuedFraction = ContinuedFraction Integer (Integer -> Integer) Integer

    instance Show ContinuedFraction where
        show (ContinuedFraction a f p)
            | p >= 0 = "[" ++ show a ++ ";(" ++ (show $ map f [0..p - 1]) ++ ")]"
            | otherwise = "[" ++ show a ++ ";(" ++ (show $ map f [0..5]) ++ ")]"

    cfFromList :: [Integer] -> ContinuedFraction
    cfFromList (x:xs) = cycleCF x xs

    period :: ContinuedFraction -> Integer
    period (ContinuedFraction _ _ p) = p

    cycleCF :: Integer -> [Integer] -> ContinuedFraction
    cycleCF a bs = ContinuedFraction a ((bs !!) . fromIntegral . (`mod` intLen)) intLen
        where intLen = toInteger $ length bs

    e :: ContinuedFraction
    e = ContinuedFraction 2 e' (-1)
        where e' n = ([1,1] ++ intercalate [1,1] (separateList [2,4..])) !! (fromIntegral n)

    evalCF :: Integer -> ContinuedFraction -> Ratio Integer
    evalCF 0 (ContinuedFraction a _ _) = a % 1
    evalCF terms (ContinuedFraction a f _) = (a % 1) + (1 / (evalCF' 0))
        where evalCF' i
                | i < terms = ((f i) % 1) + (1 / (evalCF' (i + 1)))
                | otherwise = (f i) % 1

    approximateSquareRoot :: Integral a => a -> [a]
    approximateSquareRoot s
        | sqrtS^2 == s = [sqrtS]
        | otherwise = takeUntil (/= 2 * sqrtS) $ triplets (0, 1, sqrtS)
        where triplets (m, d, a) = a : triplets (m_n, d_n, a_n)
                  where m_n = d * a - m
                        d_n = (s - m_n^2) `div` d
                        a_n = (sqrtS + m_n) `div` d_n
              sqrtS = floor $ sqrt $ fromIntegral s

    makeCF :: Integer -> ContinuedFraction
    makeCF n = cfFromList $ approximateSquareRoot n

    data Triangle = Triangle (Int, Int) (Int, Int) (Int, Int)
        deriving (Show)

    rollDice dice sides = do
        res <- mapM (\i -> getStdRandom $ randomR (1, sides)) [1..dice]
        return $ sum res

    crossProduct2D :: (Int, Int) -> (Int, Int) -> Int
    crossProduct2D (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

    dotProduct :: Num a => [a] -> [a] -> a
    dotProduct a b = sum $ zipWith (*) a b

    sameSide :: (Int, Int) ->  (Int, Int) ->  (Int, Int) ->  (Int, Int) -> Bool
    sameSide (p1x,p1y) (p2x,p2y) (ax,ay) (bx,by) = dotProduct [cp1] [cp2] >= 0
        where cp1 = crossProduct2D ((bx - ax, by - ay)) ((p1x - ax, p1y - ay))
              cp2 = crossProduct2D ((bx - ax, by - ay)) ((p2x - ax, p2y - ay))

    pointIsInTriangle :: (Int, Int) -> Triangle -> Bool
    pointIsInTriangle p (Triangle a b c) = sameSide p a b c && sameSide p b a c && sameSide p c a b

    makeTriangle :: [(Int, Int)] -> Triangle
    makeTriangle (a:b:c:_) = Triangle a b c

    mapPair :: (a -> b) -> (a, a) -> (b, b)
    mapPair f (a, b) = (f a, f b)

    flipPair :: (a, b) -> (b, a)
    flipPair (a, b) = (b, a)

    pairRatio :: Integral a => (a, a) -> Ratio a
    pairRatio (a, b) = a % b

    sumPairs :: Num a => [(a, a)] -> (a, a)
    sumPairs [] = (0, 0)
    sumPairs ((a, b):[]) = (a, b)
    sumPairs ((a, b):xs) = (a + na, b + nb)
        where (na, nb) = sumPairs xs

    makePair :: [a] -> (a, a)
    makePair (a:b:_) = (a, b)

    makePairs :: [a] -> [(a, a)]
    makePairs [] = []
    makePairs (a:b:xs) = (a, b) : makePairs xs

    testPair :: (a -> a -> Bool) -> (a, a) -> Bool
    testPair f (a, b) = f a b

    zipTo :: (a -> b) -> [a] -> [(a, b)]
    zipTo f = map (\i -> (i, f i))

    unduplicate :: [(Int, a)] -> [a]
    unduplicate [] = []
    unduplicate ((i, x):xs) = replicate i x ++ unduplicate xs

    isPermutation :: String -> String -> Bool
    isPermutation a b = sort a == sort b

    binarySearch :: (Show a, Ord a) => (a -> Ordering) -> [a] -> IO (Maybe a)
    binarySearch _ [] = return Nothing
    binarySearch f xs = do
        print $ (head secondHalf, res)
        case res of
                EQ -> return $ Just $ head secondHalf
                GT -> binarySearch f secondHalf
                LT -> binarySearch f firstHalf
        where (firstHalf, secondHalf) = halve xs
              res = f $ head secondHalf

    digits :: Integral a => a -> [a]
    digits n = digits' n
        where digits' n
                | n < 10 = [n]
                | otherwise = m : digits' d
                where (d, m) = n `quotRem` 10

    sumDigits :: Integral a => a -> a
    sumDigits = sum . digits

    flatten :: [[a]] -> [a]
    flatten = foldl (++) []

    separateList :: [a] -> [[a]]
    separateList [] = []
    separateList (x:xs) = [x] : separateList xs

    combinationElements :: [[a]] -> [[a]]
    combinationElements (x:[]) = [[i] | i <- x]
    combinationElements (x:xs) = [i : nc | i <- x, nc <- combinationElements xs]

    sequences :: Integral b => [a] -> b -> [[a]]
    sequences xs 1 = separateList xs
    sequences xs i = [x : s | x <- xs, s <- sequences xs (i - 1)]

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

    insertAll [] xs = xs
    insertAll (e:es) xs = insertAll es (insert e xs)

    insertAllBy f [] xs = xs
    insertAllBy f (e:es) xs = insertAllBy f es (insertBy f e xs)

    sumPattern ns pattern = sum (zipWith (*) ns fullPattern)
        where fullPattern = (concat . take (length ns) . repeat) pattern

    pentagonal n = n * (3 * n - 1) `div` 2

    pentagonalNumbers limit = takeWhile (<= limit) (map pentagonal znonzero)

    intersperseBy _ [] = []
    intersperseBy f (l:ls) = l ++ [f (last l)] ++ intersperseBy f ls

    n = [1..]

    z = 0 : [y | n <- [1..], y <- [n, (-n)]]

    znonzero = tail z

    points = [(x, y) | n <- [1..], x <- take n z, y <- take n z]

    divides b a = a `mod` b == 0

    mapWhile :: (a -> b) -> (b -> Bool) -> [a] -> [b]
    mapWhile f pred = takeWhile pred . map f

    takeUntil :: (a -> Bool) -> [a] -> [a]
    takeUntil _ [] = []
    takeUntil f (x:xs)
        | f x = x : takeUntil f xs
        | otherwise = [x]

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

    count :: Integral b => (a -> Bool) -> [a] -> b
    count _ [] = 0
    count f (x:xs)
        | f x = 1 + count f xs
        | otherwise = count f xs

    countDuplicates :: (Eq a, Integral b) => [a] -> [(b, a)]
    countDuplicates xs = countDuplicatesBy id xs

    countDuplicatesBy :: (Eq a, Eq b, Integral c) => (a -> b) -> [a] -> [(c, b)]
    countDuplicatesBy _ [] = []
    countDuplicatesBy f (x:xs) = (count (\i -> f i == f x) xs + 1, f x) : (countDuplicatesBy f $ filter (\i -> f i /= f x) xs)

    differences :: Num a => [a] -> [a]
    differences (a:b:[]) = [b - a]
    differences (a:b:bs) = (b - a) : differences (b : bs)

    ratioTo :: Integral a => a -> [a] -> [Ratio a]
    ratioTo test (a:[]) = [a % test]
    ratioTo test (a:as) = (a % test) : ratioTo test as

    ratios :: Integral a => [a] -> [Ratio a]
    ratios (a:b:[]) = [b % a]
    ratios (a:b:bs) = (b % a) : ratios (b : bs)

    evalRatio :: (Integral a, Fractional b) => Ratio a -> b
    evalRatio f = (fromIntegral $ numerator f) / (fromIntegral $ denominator f)

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

    padL :: a -> Int -> [a] -> [a]
    padL e i xs
        | length xs >= i = xs
        | otherwise = padL e i (e : xs)

    padR :: a -> Int -> [a] -> [a]
    padR e i xs
        | length xs >= i = xs
        | otherwise = padR e i (xs ++ [e])

    factorial n = product [1..n]

    nestedRadical f = unfoldr nestedRadical' (0, 0)
        where nestedRadical' (a, b) = Just (nextVal, (nextVal, b + 1))
                where nextVal = sqrt (a + (f b))

    series f k total = nextVal : series f (k + 1) nextVal
        where nextVal = f k + total

    showApproxFunc f = mapM_ (\i -> putStrLn (show (f i))) [1..]

    showSeries f k total = do
        mapM_ (\i -> putStr ("\r" ++ show (fromRational i) ++ "                 ")) (series f k total)
