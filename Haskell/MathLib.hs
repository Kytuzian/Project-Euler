module MathLib (printFilterMap, showMaxBy, noBuffer,
                factorial, choose, permute,
                sieve, factor, d,
                toDigits, fromDigits,
                perms,
                isSquare) where
    import Lib (remove)
    import qualified Data.Map as Map

    import System.IO

    noBuffer = hSetBuffering stdout NoBuffering

    printFilterMap :: (Show b, Show c) => (a -> Bool) -> (a -> b) -> (a -> c) -> [a] -> IO ()
    printFilterMap f trueS falseS vs = mapM_ printFilterMap' vs
        where printFilterMap' v = case f v of
                                      True -> putStrLn ("\r" ++ (show $ trueS v))
                                      False -> putStr ("\r" ++ (show $ falseS v))

    showMaxBy :: (Show a, Ord b) => (a -> b) -> [a] -> IO a
    showMaxBy f (v:vs) = do
        print v
        showMaxBy' v vs
        where showMaxBy' cur [] = return cur
              showMaxBy' cur (x:xs)
                | f x > f cur = do
                    print x
                    showMaxBy' x xs
                | otherwise = showMaxBy' cur xs

    permute :: Integral a => a -> a -> a
    n `permute` k = (factorial n) `div` (factorial (n - k))

    choose :: Integral a => a -> a -> a
    n `choose` k = (n `permute` k) `div` (factorial k)

    factorial :: Integral a => a -> a
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

    primeCandidates = spin wheel 11
        where wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
              spin (x:xs) n = n : spin xs (n + x)

    -- sieve :: Integral a => a -> [a]
    sieve limit = 2 : 3 : 5 : 7 : sieve' (Map.fromList $ zip (takeWhile (<= limit) primeCandidates) (repeat True)) 11
        where sieve' inMap n = case nextN^2 < limit of
                                    False -> nextN : (map fst $ filter snd $ Map.toList nextMap)
                                    True -> nextN : sieve' nextMap nextN
                where curMap = foldl (\cur k -> Map.delete k cur) inMap [n * n, n * n + n..limit]
                      ((nextN, _), nextMap) = Map.deleteFindMin curMap

    isSquare n
        | (n `mod` 10) `elem` [2,3,7,8] = False
        | otherwise = (floor $ sqrt $ fromIntegral n)^2 == n

    perms :: (Eq a, Num a, Eq b) => a -> [b] -> [[b]]
    perms 1 xs = map (:[]) xs
    perms _ [x] = [[x]]
    perms i xs = [x : p | x <- xs, p <- perms (i - 1) (remove xs x)]

    fromDigits :: Integral a => [a] -> a
    fromDigits (d:ds) = foldl (\a b -> 10 * a + b) d ds

    toDigits :: Integral a => a -> [a]
    toDigits n = reverse $ toDigits' n
        where toDigits' 0 = []
              toDigits' v = r : toDigits' d
                where (d, r) = v `divMod` 10

    -- factor :: Integer -> [(Integer, Int)]
    factor n = factor' n (2:[3,5..])
        where factor' 1 _ = []
              factor' cur [] = [(cur, 1)]
              factor' cur (i:is)
                | i * i > n = [(cur, 1)]
                | count > 0 = (i, count) : factor' nextCur is
                | otherwise = factor' nextCur is
                where (nextCur, count) = getDivCount cur 0
                      getDivCount v count
                        | r == 0 = getDivCount nextV (count + 1)
                        | otherwise = (v, count)
                        where (nextV, r) = v `divMod` i

    -- d :: Integral a => a -> Int
    d n = product $ map ((+ 1) . snd) $ factor n
