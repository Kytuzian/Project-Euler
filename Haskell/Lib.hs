module Lib (memoize,
            (!!!), inMatrix,
            n, z, znonzero,
            mapPair, flipPair, pairRatio, makePair, makePairs, testPair, sumPairs, pairOverlap, unPair,
            sumPair, eitherInPair,
            third,
            sumPattern, sumDigits, digits, fromDigits,
            pentagonal, pentagonalNumbers, triangleNumber,
            quadratic, intQuadratic,
            nubOnSorted, mapWhile, zipTo, takeUntil,
            combinationElements, sequences, permutationsOf,
            divides,
            defaultIfNothing,
            intersperseBy, groupFromStart, groupOverlap,
            count, countDuplicates, countDuplicatesBy, unduplicate,
            remove, removeAll, setAt, setAt2, insertReplace, insertAll, insertAllBy,
            flatten, separateList,
            startsWith, startsWithAny,
            differences, ratios, ratioTo, evalRatio,
            factorial,
            isPermutation,
            crossProduct2D, dotProduct,
            makeTriangle, sameSide, pointIsInTriangle,
            binarySearch,
            rollDice,
            intSqrt, isSquare, squares,
            padL, padR,
            approximateSquareRoot,
            truncates,
            factor, primeTau,
            pascalsTriangle,
            allProducts,
            sumProperDivisors,
            showProgressZipped, showProgress,
            incrementAt, incrementDigitsIf, incrementDigitsToIf,
            ContinuedFraction, cfFromList, cycleCF, evalCF, period, squareRootCF, makeCF,
            minimumIndex, maximumIndex, minimumIndexBy, maximumIndexBy,
            Tree, allTrees,
            distance,
            filterMap) where

    import Data.List
    import Data.List.Split (chunksOf, splitOn)
    import Data.Ratio

    import qualified Data.Map as Map
    import qualified Data.Set as Set
    import Data.IORef

    import System.IO.Unsafe
    import System.Random

    import System.ProgressBar

    import qualified Math.NumberTheory.Primes.Factorisation as Factorisation

    combinePair :: (a -> b) -> (b -> b -> c) -> (a, a) -> c
    combinePair f c (a, b) = (f a) `c` (f b)

    bothInPair :: (a -> Bool) -> (a, a) -> Bool
    bothInPair f pair = combinePair f (&&) pair

    eitherInPair :: (a -> Bool) -> (a, a) -> Bool
    eitherInPair f pair = combinePair f (||) pair

    filterMap :: (a -> Maybe b) -> [a] -> [b]
    filterMap _ [] = []
    filterMap f (x:xs) = case f x of
                            Nothing -> filterMap f xs
                            Just v -> v : filterMap f xs

    distance :: (Integral a, Floating b) => (a, a) -> (a, a) -> b
    distance (x1, y1) (x2, y2) = sqrt $ (x1f - x2f)**2 + (y1f - y2f)**2
        where (x1f, y1f) = (fromIntegral x1, fromIntegral y1)
              (x2f, y2f) = (fromIntegral x2, fromIntegral y2)

    data Tree a = Node a [Tree a] | Value a

    allTrees :: Tree a -> [[a]]
    allTrees (Value a) = [[a]]
    allTrees (Node a ts) = [a : at | t <- map allTrees ts, at <- t]

    sumPair :: Num a => (a, a) -> (a, a) -> (a, a)
    (a1, b1) `sumPair` (a2, b2) = (a1 + a2, b1 + b2)

    inMatrix :: [[a]] -> (Int, Int) -> Bool
    inMatrix matrix (x, y)
        | x < 0 || y < 0 = False
        | length matrix <= y = False
        | length (matrix !! y) <= x = False
        | otherwise = True

    (!!!) :: Integral b => [[a]] -> (b, b) -> a
    xss !!! cs = (xss !! y) !! x
        where (x, y) = mapPair fromIntegral cs

    wordList = do
        contents <- readFile "/usr/share/dict/words"
        return $ splitOn "\n" contents

    incrementAt :: Integral a => [a] -> Int -> [a]
    incrementAt xs i = take i xs ++ [e + 1] ++ drop (i + 1) xs
        where e = xs !! i

    incrementDigitsToIf :: Integral a => a -> ([a] -> Bool) -> [a] -> [a]
    incrementDigitsToIf v f ns = incrementDigitsToIf' ns 0
        where incrementDigitsToIf' ds i
                | i >= length ds = ds ++ [0]
                | f (incrementAt ds i) = incrementAt ds i
                | otherwise = incrementDigitsToIf' (setAt ds i v) (i + 1)

    incrementDigitsIf :: Integral a => ([a] -> Bool) -> [a] -> [a]
    incrementDigitsIf f ns = incrementDigitsToIf 0 f ns

    showProgressZipped limit res = mapM_ (\(i, _) -> progressBar (msg (show i ++ " of " ++ show limit)) percentage 80 i limit) res
    showProgress limit res = mapM_ (\i -> progressBar (msg (show i ++ " of " ++ show limit)) percentage 80 i limit) res

    sumProperDivisors :: Integral a => a -> a
    sumProperDivisors n = fromIntegral $ sum $ init $ Set.toList $ Factorisation.divisors $ fromIntegral n

    allProducts :: (Ord a, Num a) => a -> [a] -> [a]
    allProducts _ [] = []
    allProducts limit (n:ns) = allProducts' (n:ns) ++ allProducts limit ns
        where allProducts' [] = []
              allProducts' (x:xs)
                | n * x > limit = []
                | otherwise = n * x : allProducts' xs

    primeTau :: Integral a => a -> a
    primeTau n = fromIntegral $ snd $ sumPairs $ Factorisation.factorise $ fromIntegral n

    factor :: Integral a => a -> [a]
    factor n = map fromIntegral $ unduplicate $ map flipPair $ Factorisation.factorise $ fromIntegral n

    startsWith :: Eq a => [a] -> [a] -> Bool
    startsWith [] [] = True
    startsWith [] _ = False
    startsWith _ [] = True
    startsWith (a:as) (b:bs)
        | a == b = startsWith as bs
        | otherwise = False

    startsWithAny :: Eq a => [a] -> [[a]] -> Bool
    startsWithAny s ps = any (startsWith s) ps

    partialSums :: Num a => [a] -> [a]
    partialSums xs = partialSums' 0 xs
        where partialSums' i [] = [i]
              partialSums' i (x:xs) = i + x : partialSums' (i + x) xs

    truncates :: Integral a => a -> [a]
    truncates n = truncates' $ init $ reverse $ digits n
        where truncates' [] = []
              truncates' xs = fromDigits xs : truncates' (init xs)

    isSquare :: Integral a => a -> Bool
    isSquare n = (intSqrt n)^2 == n

    squares = map (^2) [1..]

    third :: (a, b, c) -> c
    third (_, _, c) = c

    intSqrt :: Integral a => a -> a
    intSqrt = floor . sqrt . fromIntegral

    pascalsTriangle :: Integral a => [[a]]
    pascalsTriangle = pascalsTriangle' [1]
        where pascalsTriangle' ns = ns : pascalsTriangle' nextNs
                where nextNs = 1 : ((map (\(a, b) -> a + b) $ pairOverlap ns) ++ [1])

    pairOverlap :: [a] -> [(a, a)]
    pairOverlap [] = []
    pairOverlap (x:[]) = []
    pairOverlap (x1:x2:xs) = (x1, x2) : pairOverlap (x2:xs)

    unPair :: (a, a) -> [a]
    unPair (a, b) = [a, b]

    groupOverlap :: Int -> [a] -> [[a]]
    groupOverlap _ [] = []
    groupOverlap len xs = take len xs : (groupOverlap len $ tail xs)

    groupFromStart :: Int -> [a] -> [[a]]
    groupFromStart len l@(x:xs)
        | length l `mod` 2 > 0 = [x] : groupFromStart' xs
        | otherwise = groupFromStart' l
        where groupFromStart' [] = []
              groupFromStart' xs = take len xs : (groupFromStart' $ drop len xs)

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

    approximateSquareRoot :: Integer -> [Integer]
    approximateSquareRoot s
        | isSquare s = reverse $ digits $ intSqrt s
        | otherwise = firstN : approximateSquareRoot' firstN firstPart firstXs
        where firstN = intSqrt $ fromDigits $ head sDigits
              firstPart = (fromDigits $ head sDigits) - firstN^2
              firstXs = tail sDigits ++ chunksOf 2 (repeat 0)
              sDigits = groupFromStart 2 $ reverse $ digits s
              approximateSquareRoot' guess part ((x1:x2:[]):xs) = nextN : approximateSquareRoot' nextGuess (curPart - (2 * guess * 10 + nextN) * nextN) xs
                where nextN = findD (2 * guess) curPart
                      nextGuess = guess * 10 + nextN
                      curPart = part * 100 + x1*10 + x2
              findD x target = case find (\i -> i * (x * 10 + i) <= target) [9,8..1] of
                                 Just a -> a
                                 Nothing -> 0

    squareRootCF :: Integral a => a -> [a]
    squareRootCF s
        | sqrtS^2 == s = [sqrtS]
        | otherwise = takeUntil (/= 2 * sqrtS) $ triplets (0, 1, sqrtS)
        where triplets (m, d, a) = a : triplets (m_n, d_n, a_n)
                  where m_n = d * a - m
                        d_n = (s - m_n^2) `div` d
                        a_n = (sqrtS + m_n) `div` d_n
              sqrtS = floor $ sqrt $ fromIntegral s

    makeCF :: Integer -> ContinuedFraction
    makeCF n = cfFromList $ squareRootCF n

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

    sumPairs :: (Num a, Num b) => [(a, b)] -> (a, b)
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

    fromDigits :: Integral a => [a] -> a
    fromDigits [] = 0
    fromDigits (x:xs) = x * 10^(length xs) + fromDigits xs

    digits :: Integral a => a -> [a]
    digits n = digits' n
        where digits' n
                | n < 10 = [n]
                | otherwise = m : digits' d
                where (d, m) = n `quotRem` 10

    sumDigits :: Integral a => a -> a
    sumDigits = sum . digits

    flatten :: [[a]] -> [a]
    flatten [] = []
    flatten (x:xs) = x ++ flatten xs

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

    minimumIndexBy :: Ord b => (a -> b) -> [a] -> (Int, a)
    minimumIndexBy f (x:xs) = (i, a)
        where (a, i) = foldl cmpf (x, 0) $ zip xs [1..]
              cmpf (a, ai) (b, bi)
                | f a < f b = (a, ai)
                | otherwise = (b, bi)

    minimumIndex :: (Ord a) => [a] -> (Int, a)
    minimumIndex xs = minimumIndexBy id xs

    maximumIndex :: (Ord a) => [a] -> (Int, a)
    maximumIndex xs = maximumIndexBy id xs

    maximumIndexBy :: Ord b => (a -> b) -> [a] -> (Int, a)
    maximumIndexBy f (x:xs) = (i, a)
        where (a, i) = foldl cmpf (x, 0) $ zip xs [1..]
              cmpf (a, ai) (b, bi)
                | f a > f b = (a, ai)
                | otherwise = (b, bi)

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

    count :: Num b => (a -> Bool) -> [a] -> b
    count _ [] = 0
    count f (x:xs)
        | f x = 1 + count f xs
        | otherwise = count f xs

    countDuplicates :: (Eq a, Num b) => [a] -> [(b, a)]
    countDuplicates xs = countDuplicatesBy id xs

    countDuplicatesBy :: (Eq a, Eq b, Num c) => (a -> b) -> [a] -> [(c, b)]
    countDuplicatesBy _ [] = []
    countDuplicatesBy f (x:xs) = (count (\i -> f i == f x) xs + 1, f x) : (countDuplicatesBy f $ filter (\i -> f i /= f x) xs)

    permutationsOf :: (Eq a, Integral b) => [a] -> b -> [[a]]
    permutationsOf xs 1 = separateList xs
    permutationsOf xs i = [x : s | x <- xs, s <- permutationsOf (remove xs x) (i - 1)]

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

    setAt2 :: [[a]] -> (Int, Int) -> a -> [[a]]
    setAt2 xs (x, y) e = take y xs ++ [(setAt (xs !! y) x e)] ++ drop (y + 1) xs

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
